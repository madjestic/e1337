{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE DeriveGeneric #-}
module Main where 

import Control.Concurrent
import Control.Lens       hiding (transform)
import Data.Text          (Text)
import Foreign.C          
import FRP.Yampa          hiding (identity)
import Data.Functor       (($>))

import Linear.Matrix      (M44, M33, identity)
import SDL                hiding ( Point
                                 , M44
                                 , M33
                                 , Event
                                 , (^+^)
                                 , (*^))
import Camera        as Cam
import Game          
import Keys          
import Object        as Obj
import Controllable  
import Geometry      
import Input         as Inp
import Rendering
import Data.Foldable as DF (toList)
import GHC.Generics

import Debug.Trace   as DT

--        d8888888b    8888888888888b     d888       d8888888888888888888888888 
--       d888888888b   888  888  8888b   d8888      d88888    888    888        
--      d88P88888888b  888  888  88888b.d88888     d88P888    888    888        
--     d88P 888888Y88b 888  888  888Y88888P888    d88P 888    888    8888888    
--    d88P  888888 Y88b888  888  888 Y888P 888   d88P  888    888    888        
--   d88P   888888  Y88888  888  888  Y8P  888  d88P   888    888    888        
       --  d8888888888888   Y8888  888  888   "   888 d8888888888    888    888        
-- d88P     888888    Y8888888888888       888d88P     888    888    8888888888 

-- < Animate > ------------------------------------------------------------
type WinInput = Event SDL.EventPayload
type WinOutput = (Game, Bool)

animate :: SDL.Window
        -> Descriptor
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate window resources sf =
  do
    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf
    closeWindow window
    
      where
        senseInput _ =
          do
            lastInteraction <- newMVar =<< SDL.time
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent                          
            return (dt, Event . SDL.eventPayload <$> mEvent) 

        renderOutput _ (game, shouldExit) =
          do
            uniforms <- initUniforms game
            draw window resources
            return shouldExit 


--  .d8888b.88888888888    d8888888888888888888888888888b     d888       d8888 .d8888b. 888    8888888888888b    8888888888888 
-- d88P  Y88b   888       d88888    888    888       8888b   d8888      d88888d88P  Y88b888    888  888  8888b   888888        
-- Y88b.        888      d88P888    888    888       88888b.d88888     d88P888888    888888    888  888  88888b  888888        
--  "Y888b.     888     d88P 888    888    8888888   888Y88888P888    d88P 888888       8888888888  888  888Y88b 8888888888    
--     "Y88b.   888    d88P  888    888    888       888 Y888P 888   d88P  888888       888    888  888  888 Y88b888888        
--       "888   888   d88P   888    888    888       888  Y8P  888  d88P   888888    888888    888  888  888  Y88888888        
-- Y88b  d88P   888  d8888888888    888    888       888   "   888 d8888888888Y88b  d88P888    888  888  888   Y8888888        
--  "Y8888P"    888 d88P     888    888    8888888888888       888d88P     888 "Y8888P" 888    8888888888888    Y8888888888888 

-- < Game Logic > ---------------------------------------------------------
mainGame :: Game -> SF AppInput Game
mainGame initGame = 
  loopPre initGame $ 
  proc (input, game) -> do
    gs <- case gStg game of
            GameIntro   -> gameIntro   -< (input, game)
            GamePlaying -> gamePlay initGame -< input
    returnA -< (gs, gs)

gameIntro :: SF (AppInput, Game) Game
gameIntro =
  switch sf cont        
     where sf =
             proc (input, game) -> do
               introState <- returnA -< game
               playState  <- returnA -< game { gStg =  GamePlaying }
               skipE      <- key SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after loadDelay () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> playState)
           cont game  = 
             proc input -> returnA -< game

gamePlay :: Game -> SF AppInput Game
gamePlay game =
    switch sf (const (mainGame game))        
     where sf =
             proc input -> do
               game      <- updateGame game -< input
               reset     <- key SDL.ScancodeSpace "Pressed" -< input
               returnA   -< (game, reset)

updateGame :: Game -> SF AppInput Game
updateGame game = 
  proc input -> do
    --obj <- updateObject $ object game -< input
    let obj = object game
    cam <- updateCamera $ camera game -< input
    returnA -< Game GamePlaying obj cam

updateCamera :: Camera -> SF AppInput Camera
updateCamera cam =
  proc input -> do
    ctl <- control
           ( Controllable
             ((transform . Cam.controller) cam)
             ((ypr       . Cam.controller) cam)
             ((keys      . Cam.controller) cam)
             ((keyVecs   . Cam.controller) cam)) -< input
      
    returnA -< Camera ctl
    
updateObject :: Object -> SF AppInput Object
updateObject obj =
  proc input -> do
    sclr    <- updateScalar $ scalar obj -< input
    --mtx     <- controller (Object.transform obj) (velocity obj) (keys obj) -< input
    --returnA -< Object sclr (geometry obj) mtx (velocity obj) (keys obj)
    returnA -<
      Object
      sclr
      (geometry obj)
      (velocity obj)
      ( Controllable
        ((transform . Obj.controller) obj)
        ((ypr       . Obj.controller) obj)
        ((keys      . Obj.controller) obj)
        ((keyVecs   . Obj.controller) obj))

auxF :: SDL.Scancode -> (Keys -> Bool) -> Controllable -> SF AppInput (Bool, Event ())
auxF code key ctl =
  proc input -> do
    keyPressed     <- key' code  "Pressed"  -< input
    keyReleased    <- key' code  "Released" -< input
    let result = keyEvent (key (keys ctl)) keyPressed keyReleased
        event  = lMerge keyPressed keyReleased
    returnA -< (result, event)

updateKeys :: Controllable -> SF AppInput (Keys, [Event ()])
updateKeys ctl0 =
  proc input -> do

    keyWp     <- key' SDL.ScancodeW     "Pressed"  -< input
    keyWr     <- key' SDL.ScancodeW     "Released" -< input
    -- let keyW_ = keyEvent (keyW     keys0) keyWp      keyWr
    let keys0 = keys ctl0
    
    (keyW_, keyWe) <- auxF SDL.ScancodeW keyW ctl0 -< input
    (keyS_, keySe) <- auxF SDL.ScancodeS keyS ctl0 -< input
    
    keySp     <- key' SDL.ScancodeS     "Pressed"  -< input
    keySr     <- key' SDL.ScancodeS     "Released" -< input
    keyAp     <- key' SDL.ScancodeA     "Pressed"  -< input
    keyAr     <- key' SDL.ScancodeA     "Released" -< input
    keyDp     <- key' SDL.ScancodeD     "Pressed"  -< input
    keyDr     <- key' SDL.ScancodeD     "Released" -< input
                                                  
    keyQp     <- key' SDL.ScancodeQ     "Pressed"  -< input
    keyQr     <- key' SDL.ScancodeQ     "Released" -< input
    keyEp     <- key' SDL.ScancodeE     "Pressed"  -< input
    keyEr     <- key' SDL.ScancodeE     "Released" -< input
    keyZp     <- key' SDL.ScancodeZ     "Pressed"  -< input
    keyZr     <- key' SDL.ScancodeZ     "Released" -< input
    keyXp     <- key' SDL.ScancodeX     "Pressed"  -< input
    keyXr     <- key' SDL.ScancodeX     "Released" -< input
                     
    keyUpP    <- key SDL.ScancodeUp    "Pressed"  -< input
    keyUpR    <- key SDL.ScancodeUp    "Released" -< input
    keyDownP  <- key SDL.ScancodeDown  "Pressed"  -< input
    keyDownR  <- key SDL.ScancodeDown  "Released" -< input
    keyLeftP  <- key SDL.ScancodeLeft  "Pressed"  -< input
    keyLeftR  <- key SDL.ScancodeLeft  "Released" -< input
    keyRightP <- key SDL.ScancodeRight "Pressed"  -< input
    keyRightR <- key SDL.ScancodeRight "Released" -< input

    events <-
      returnA -<
      [ keyWe
      , keySe ]

    -- events <-
    --   returnA -<
    --   [ keyWp,     keyWr
    --   , keySp,     keySr
    --   , keyAp,     keyAr
    --   , keyDp,     keyDr
    --   , keyQp,     keyQr
    --   , keyEp,     keyEr 
    --   , keyZp,     keyZr 
    --   , keyXp,     keyXr
    --   , keyUpP,    keyUpR
    --   , keyDownP,  keyDownR
    --   , keyLeftP,  keyLeftR
    --   , keyRightP, keyRightR ]
      
    keys <-
      returnA -<
      (Keys
        --( keyEvent (keyW     keys0) keyWp      keyWr     )
        keyW_
        --( keyEvent (keyS     keys0) keySp      keySr     )
        keyS_
        ( keyEvent (keyA     keys0) keyAp      keyAr     )
        ( keyEvent (keyD     keys0) keyDp      keyDr     )
        ( keyEvent (keyQ     keys0) keyQp      keyQr     )
        ( keyEvent (keyE     keys0) keyEp      keyEr     )
        ( keyEvent (keyZ     keys0) keyZp      keyZr     )
        ( keyEvent (keyX     keys0) keyXp      keyXr     )
        ( keyEvent (keyUp    keys0) keyUpP     keyUpR    )
        ( keyEvent (keyDown  keys0) keyDownP   keyDownR  )
        ( keyEvent (keyLeft  keys0) keyLeftP   keyLeftR  )
        ( keyEvent (keyRight keys0) keyRightP  keyRightR ))

    returnA -< (keys, events)

instance VectorSpace (V3 Double) Double where
  zeroVector                   = (V3 0 0 0)
  (*^) s (V3 x y z)            = (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3 x y z) (V3 k l m) = (V3 (x+k) (y+l) (z+m))
  dot    (V3 x y z) (V3 k l m) = (x*k) + (y*l) + (z*m)

control :: Controllable -> SF AppInput Controllable
control ctl0 =
  -- | foldrWith mtx0 keys - for every key apply a folding transform to mtx0
  -- | in case of keypress event - update the set of keys and call new fold ^
  switch sf cont
    where
      sf = proc input -> do
        ctl         <- update ctl0            -< ctl0
        mtx         <- returnA                -< transform ctl
        ypr         <- returnA                -< ypr       ctl
        (keys, evs)  <- updateKeys ctl0 -< input

        result <-
          returnA -<
          ( Controllable
            mtx
            ypr
            keys
            (keyVecs ctl) )
        
        returnA -< 
          ( ctl
          , catEvents evs
            $> result) -- :: (Controllable, Event Controllable)
      cont result = control result

keyEvent :: Bool -> Event () -> Event () -> Bool
keyEvent state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise = state

update :: Controllable -> SF Controllable Controllable
update ctl0 = iterFrom update1 ctl0
  where
    update1 :: Controllable -> Controllable -> DTime -> Controllable -> Controllable
    update1 ctl0 ctl1 dt ctl2 = ctl
      where
        ctl = (Controllable mtx ypr keys0 (keyVecs ctl0))
          where
            mtx0  = (transform ctl2)
            keys0 = (keys      ctl0)

            ypr :: V3 Double
            ypr   =
              (99999 * ) $
              ((V3 dt dt dt) * ) $
              foldr (+) (V3 0 0 0) $
              zipWith (*^) ((\x -> if x then 1 else 0) . ($ keys0) <$>
                            [ keyUp,  keyDown, keyLeft, keyRight, keyQ,  keyE ])
                            [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
              where
                pPitch = (keyVecs ctl0)!!6  -- positive  pitch
                nPitch = (keyVecs ctl0)!!7  -- negative  pitch
                pYaw   = (keyVecs ctl0)!!8  -- positive  yaw
                nYaw   = (keyVecs ctl0)!!9  -- negative  yaw
                pRoll  = (keyVecs ctl0)!!10 -- positive  roll
                nRoll  = (keyVecs ctl0)!!11 -- negative  roll
            
            mtx =
              mkTransformationMat
              rot
              tr
              where
                rot =
                  (view _m33 mtx0)
                  !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr)) -- yaw
                  !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr)) -- pitch
                  !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr)) -- roll

                tr  =
                  foldr (+) (view translation mtx0) $
                  fmap (0.1 *) $
                  fmap (transpose (rot) !*) $
                  zipWith (*^) ((\x -> if x then 1 else 0) . ($ keys0) <$>
                                [keyW, keyS, keyA, keyD, keyZ, keyX])
                                [fVel, bVel, lVel, rVel, uVel, dVel]

                  where fVel   = (keyVecs ctl0)!!0  -- forwards  velocity
                        bVel   = (keyVecs ctl0)!!1  -- backwards velocity
                        lVel   = (keyVecs ctl0)!!2  -- left      velocity
                        rVel   = (keyVecs ctl0)!!3  -- right     velocity
                        uVel   = (keyVecs ctl0)!!4  -- right     velocity
                        dVel   = (keyVecs ctl0)!!5  -- right     velocity

updateScalar :: Double -> SF AppInput Double
updateScalar pp0 =
  switch sf cont
    where
      sf = proc input -> do
        --_ <- DT.trace ("updateScalar: " ++ show pp0) $ returnA -< ()
        keyLeft  <- key SDL.ScancodeLeft  "Pressed" -< input
        keyRight <- key SDL.ScancodeRight "Pressed" -< input
        let result :: ( Double
                      , Event ()
                      , Event ())
            result =  ( pp0
                      , keyLeft
                      , keyRight )
        returnA -< (pp0, mergeEvents
                         [ keyLeft
                         , keyRight ] $> result)

      cont (x, keyLeft, keyRight) =
        if | isEvent keyLeft -> updateScalar' x (-0.5)
           | otherwise       -> updateScalar' x   0.5

updateScalar' :: Double -> Double -> SF AppInput Double
updateScalar' pp0 v0 =
  switch sf cont
    where
         sf =
           proc input -> do
             p       <- -- DT.trace ("p: " ++ show pp0 ++ "\n") $
                        (pp0 +) ^<< integral -< v0
             keyLeft <- key SDL.ScancodeLeft  "Released" -< input
             keyRight<- key SDL.ScancodeRight "Released" -< input
             returnA -< (p, mergeEvents
                            [ keyLeft  
                            , keyRight ] $> p) :: (Double, Event Double)
         cont = updateScalar

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

jsonFile :: FilePath
jsonFile = "src/model.pgeo"

--  CCCCC   OOOOO  NN   NN  SSSSS  TTTTTTT   AAA   NN   NNn TTTTTTT  SSSSS  
-- CC    C OO   OO NNN  NN SS        TTT    AAAAA  NNN  NN   TTT   SS      
-- CC      OO   OO NN N NN  SSSSS    TTT   AA   AA NN N NN   TTT    SSSSS  
-- CC    C OO   OO NN  NNN      SS   TTT   AAAAAAA NN  NNN   TTT        SS 
--  CCCCC   OOOO0  NN   NN  SSSSS    TTT   AA   AA NN   NN   TTT    SSSSS  

-- < Global Constants > ---------------------------------------------------
mBlur     = 0.25 :: Float
loadDelay = 2.0  :: Double
resX      = 800  :: CInt
resY      = 600  :: CInt

initGame :: IO Game
initGame =
  do
    geo <- readPGeo jsonFile
    let obj =
          Object
          0.0
          geo
          (V4 0 0 0 0)
          ( Controllable
            (identity :: M44 Double)
            (V3 0 0 0)
            (Keys
             False
             False
             False
             False
             False
             False
             False
             False
             False
             False
             False
             False)
            [ fVel 
            , bVel 
            , lVel 
            , rVel 
            , uVel 
            , dVel 
            , pPitch
            , nPitch
            , pYaw 
            , nYaw 
            , pRoll
            , nRoll ])
          where
            fVel   = V3 ( 0  )( 0  )( 999)   -- forwards  velocity
            bVel   = V3 ( 0  )( 0  )(-999)   -- backwards velocity
            lVel   = V3 ( 999)( 0  )( 0  )   -- left      velocity
            rVel   = V3 (-999)( 0  )( 0  )   -- right     velocity
            uVel   = V3 ( 0  )(-999)( 0  )   -- right     velocity
            dVel   = V3 ( 0  )( 999)( 0  )   -- right     velocity
            pPitch = V3 ( 999)( 0  )( 0  )   -- positive  pitch
            nPitch = V3 (-999)( 0  )( 0  )   -- negative  pitch
            pYaw   = V3 ( 0  )( 999)( 0  )   -- positive  yaw
            nYaw   = V3 ( 0  )(-999)( 0  )   -- negative  yaw
            pRoll  = V3 ( 0  )(  0 )( 999)   -- positive  roll
            nRoll  = V3 ( 0  )(  0 )(-999)   -- negative  roll
            
        cam =
          Camera
          ( Controllable
            (identity :: M44 Double)
            (V3 0 0 0)
            (Keys
             False
             False
             False
             False
             False
             False
             False
             False
             False
             False
             False
             False )
            [ fVel 
            , bVel 
            , lVel 
            , rVel 
            , uVel 
            , dVel 
            , pPitch
            , nPitch
            , pYaw 
            , nYaw 
            , pRoll
            , nRoll ])
          where
            fVel   = V3 ( 0  )( 0  )( 0.1)   -- forwards  velocity
            bVel   = V3 ( 0  )( 0  )(-0.1)   -- backwards velocity
            lVel   = V3 ( 0.1)( 0  )( 0  )   -- left      velocity
            rVel   = V3 (-0.1)( 0  )( 0  )   -- right     velocity
            uVel   = V3 ( 0  )(-0.1)( 0  )   -- right     velocity
            dVel   = V3 ( 0  )( 0.1)( 0  )   -- right     velocity
            pPitch = V3 ( 0.1)( 0  )( 0  )   -- positive  pitch
            nPitch = V3 (-0.1)( 0  )( 0  )   -- negative  pitch
            pYaw   = V3 ( 0  )(-0.1)( 0  )   -- positive  yaw
            nYaw   = V3 ( 0  )( 0.1)( 0  )   -- negative  yaw
            pRoll  = V3 ( 0  )(  0 )(-0.1)   -- positive  roll
            nRoll  = V3 ( 0  )(  0 )( 0.1)   -- negative  roll

        initGame = Game GamePlaying obj cam
    return initGame


-- 888b     d888       d88888888888888b    888 
-- 8888b   d8888      d88888  888  8888b   888 
-- 88888b.d88888     d88P888  888  88888b  888 
-- 888Y88888P888    d88P 888  888  888Y88b 888 
-- 888 Y888P 888   d88P  888  888  888 Y88b888 
-- 888  Y8P  888  d88P   888  888  888  Y88888 
-- 888   "   888 d8888888888  888  888   Y8888 
-- 888       888d88P     8888888888888    Y888 

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do
  game      <- initGame
  window    <- openWindow "e1337" (resX, resY)
  resources <- initBufferObjects game
  
  animate
    window
    resources
    (parseWinInput >>> (mainGame game &&& handleExit))
