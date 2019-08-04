{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

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
                                 , Mouse
                                 , (^+^)
                                 , (*^))
import Camera        as C
import Game          
import Keyboard
import Object        as O
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
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
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
               reset     <- keyInput SDL.ScancodeSpace "Pressed" -< input
               returnA   -< (game, reset)

updateGame :: Game -> SF AppInput Game
updateGame game = 
  proc input -> do
    let obj = object game
    --obj <- updateObject $ object game -< input
    cam <- updateCamera $ camera game -< input
    returnA -< Game GamePlaying obj cam

updateCamera :: Camera -> SF AppInput Camera
updateCamera cam = 
  proc input -> do
    kctl <- control (C.driver $ cam) -< input
    mctl <- updateMouse (C.driver $ cam) -< input
    let pos0 = debug mctl
    returnA -< Camera { C.driver = kctl { debug = pos0 } }
    
updateObject :: Object -> SF AppInput Object
updateObject obj =
  proc input -> do
    sclr    <- updateScalar $ scalar obj -< input
    --mtx     <- controller (Object.transform obj) (velocity obj) (keys obj) -< input
    --returnA -< Object sclr (geometry obj) mtx (velocity obj) (keys obj)
    returnA -< obj
      -- Object
      -- sclr
      -- (geometry obj)
      -- (velocity obj)
      -- ( Controllable
      --   ((transform . O.controller) obj)
      --   ((ypr       . O.controller) obj)
      --   ((keys      . O.controller) obj)
      --   ((keyVecs   . O.controller) obj))

keyEvent :: Bool -> Event () -> Event () -> Bool
keyEvent state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise = state

keyEvents :: SDL.Scancode -> (Keys -> Bool) -> Controllable -> SF AppInput (Bool, Event ())
keyEvents code keyFunc ctl = 
  proc input -> do
    keyPressed     <- keyInput code  "Pressed"  -< input
    keyReleased    <- keyInput code  "Released" -< input
    let keys0  = keys.keyboard.controller $ ctl
        result = keyEvent (keyFunc keys0) keyPressed keyReleased
        event  = lMerge keyPressed keyReleased
    returnA -< (result, event)

mousePosEvent :: SF AppInput ((Double, Double), Event ())
mousePosEvent = mouseEventPos &&& mouseEvent

updateKeys :: Controllable -> SF AppInput (Keys, [Event ()])
updateKeys ctl0 = 
  proc input -> do
    let keys0 = keys.keyboard.controller $ ctl0
    
    (keyW_, keyWe) <- keyEvents SDL.ScancodeW keyW ctl0 -< input
    (keyS_, keySe) <- keyEvents SDL.ScancodeS keyS ctl0 -< input
    (keyA_, keyAe) <- keyEvents SDL.ScancodeA keyA ctl0 -< input
    (keyD_, keyDe) <- keyEvents SDL.ScancodeD keyD ctl0 -< input

    (keyQ_, keyQe) <- keyEvents SDL.ScancodeQ keyQ ctl0 -< input
    (keyE_, keyEe) <- keyEvents SDL.ScancodeE keyE ctl0 -< input
    (keyZ_, keyZe) <- keyEvents SDL.ScancodeZ keyZ ctl0 -< input
    (keyX_, keyXe) <- keyEvents SDL.ScancodeX keyX ctl0 -< input
  
    (keyUp_,    keyUpE)    <- keyEvents SDL.ScancodeUp    keyUp    ctl0 -< input
    (keyDown_,  keyDownE)  <- keyEvents SDL.ScancodeDown  keyDown  ctl0 -< input
    (keyLeft_,  keyLeftE)  <- keyEvents SDL.ScancodeLeft  keyLeft  ctl0 -< input
    (keyRight_, keyRightE) <- keyEvents SDL.ScancodeRight keyRight ctl0 -< input

    let events = [      keyWe, keySe, keyAe, keyDe, keyQe, keyEe, keyZe, keyXe, keyUpE, keyDownE, keyLeftE, keyRightE  ]
        keys   = ( Keys keyW_  keyS_  keyA_  keyD_  keyQ_  keyE_  keyZ_  keyX_  keyUp_  keyDown_  keyLeft_  keyRight_ )

    returnA -< (keys, events)

instance VectorSpace (V3 Double) Double where
  zeroVector                   = (V3 0 0 0)
  (*^) s (V3 x y z)            = (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3 x y z) (V3 k l m) = (V3 (x+k) (y+l) (z+m))
  dot    (V3 x y z) (V3 k l m) = (x*k) + (y*l) + (z*m)

control :: Controllable -> SF AppInput Controllable
control ctl0 =
  -- | foldrWith mtx0 keys - for every keyInput apply a folding transform to mtx0
  -- | in case of keypress event - update the set of keys and call new fold ^
  switch sf cont
    where
      sf = proc input -> do
        ctl           <- update ctl0      -< ctl0
        mtx           <- returnA          -< transform ctl
        ypr           <- returnA          -< ypr       ctl
        (kkeys, kevs) <- updateKeys  ctl0 -< input
        (pos0, mev)   <- mousePosEvent    -< input

        result <-
          returnA -<
          ( Controllable (0,0) mtx ypr
            ( Controller
              ( Keyboard kkeys (keyVecs (keyboard (controller ctl))) )
              ( Mouse Nothing Nothing pos0 [] ) ) )
        returnA -< 
          ( ctl
          , catEvents (mev:kevs) --(kevs ++ [mevs])
            $> result) -- :: (Controllable, Event Controllable)
      cont result = control result

updateMouse :: Controllable -> SF AppInput Controllable
updateMouse ctl0 =
  switch sf cont
  where
    sf = proc input -> do
      mtx           <- returnA          -< transform ctl0
      ypr           <- returnA          -< ypr       ctl0
      (pos0, mev)   <- mousePosEvent    -< input
      result <-
        returnA -<
        ( Controllable
          pos0
          mtx
          ypr
          ( Controller
            ( keyboard.controller $ ctl0)
            ( Mouse Nothing Nothing pos0 [] ) ) )
      returnA -<
        ( ctl0
        , (DT.trace (show pos0) $ mev)
          $> result
        )
    cont result = updateMouse result

update :: Controllable -> SF Controllable Controllable
update ctl0 =
  iterFrom update1 ctl0
  where
    update1 :: Controllable -> Controllable -> DTime -> Controllable -> Controllable
    update1 ctl0 ctl1 dt ctl2 = ctl
      where
        kvs = (keyVecs.keyboard.controller $ ctl0)
        mvs = [ -- mouse vectors
                (V3 )]
        pos0 = pos.mouse.controller $ ctl2

        ctl = (Controllable
               (0,0)
               mtx
               ypr
               (Controller
                (Keyboard keys0 kvs)
                (Mouse Nothing Nothing pos0 [])))
          where
            mtx0  = (transform ctl2)
            keys0 = (keys.keyboard.controller $ ctl0)

            ypr :: V3 Double
            ypr   =
              (99999 * ) $
              ((V3 dt dt dt) * ) $
              foldr (+) (V3 0 0 0) $
              zipWith (*^) ((\x -> if x then 1 else 0) . ($ keys0) <$>
                            [ keyUp,  keyDown, keyLeft, keyRight, keyQ,  keyE ])
                            [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
              where
                pPitch = (keyVecs.keyboard.controller $ ctl0)!!6  -- positive  pitch
                nPitch = (keyVecs.keyboard.controller $ ctl0)!!7  -- negative  pitch
                pYaw   = (keyVecs.keyboard.controller $ ctl0)!!8  -- positive  yaw
                nYaw   = (keyVecs.keyboard.controller $ ctl0)!!9  -- negative  yaw
                pRoll  = (keyVecs.keyboard.controller $ ctl0)!!10 -- positive  roll
                nRoll  = (keyVecs.keyboard.controller $ ctl0)!!11 -- negative  roll
            
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

                  where fVel   = (keyVecs.keyboard.controller $ ctl0)!!0  -- forwards  velocity
                        bVel   = (keyVecs.keyboard.controller $ ctl0)!!1  -- backwards velocity
                        lVel   = (keyVecs.keyboard.controller $ ctl0)!!2  -- left      velocity
                        rVel   = (keyVecs.keyboard.controller $ ctl0)!!3  -- right     velocity
                        uVel   = (keyVecs.keyboard.controller $ ctl0)!!4  -- right     velocity
                        dVel   = (keyVecs.keyboard.controller $ ctl0)!!5  -- right     velocity

updateScalar :: Double -> SF AppInput Double
updateScalar pp0 =
  switch sf cont
    where
      sf = proc input -> do
        --_ <- DT.trace ("updateScalar: " ++ show pp0) $ returnA -< ()
        keyLeft  <- keyInput SDL.ScancodeLeft  "Pressed" -< input
        keyRight <- keyInput SDL.ScancodeRight "Pressed" -< input
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
             keyLeft <- keyInput SDL.ScancodeLeft  "Released" -< input
             keyRight<- keyInput SDL.ScancodeRight "Released" -< input
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
    let obj = initObj { geometry = geo }
        cam = initCam

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

{-

parseWinInput :: SF WinInput AppInput

&&&
mainGame      :: Game -> SF AppInput Game
handleExit    :: SF AppInput Bool
-> SF AppInput (Game, Bool)

>>>
parseWinInput :: SF WinInput AppInput
_             :: SF AppInput (Game, Bool)
-> SF WinInput (Game, Bool)

arr :: (a → b) → SF a b
(≫) :: SF a b → SF b c → SF a c
(&&&) :: SF a b → SF a c → SF a (b, c)
loop :: SF (a, c) (b, c) → SF a b

-}
