{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where 

import Control.Concurrent
import Control.Lens       hiding (transform)
import Data.Text          (Text)
import Foreign.C          
import FRP.Yampa          hiding (identity, (*^))
import Linear.Matrix
import SDL                hiding (Point, Vec2, Vec3, M44, Event, (^+^))

import Game
import Geometry
import Input
import Rendering
import Data.Foldable as DF (toList)

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
               returnA    -< (introState, (skipE `lMerge` waitE) `tag` playState)
           cont game  = 
             proc input -> do
               returnA  -< game

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
       obj <- updateObject $ object game -< input
       returnA -< Game GamePlaying obj

updateObject :: Object -> SF AppInput Object
updateObject obj =
  proc input -> do
    sclr    <- updateScalar $ scalar obj -< input
    mtx     <- updateTransform (transform obj) (velocity obj) (keys obj) -< input
    returnA -< Object sclr (geometry obj) mtx (velocity obj) (keys obj)

updateTransform :: M44 Double -> V4 Double -> Keys -> SF AppInput (M44 Double)
updateTransform mtx0 vel0 keys0 =
  -- | foldrWith mtx0 keys - for every key apply a folding transform to mtx0
  -- | in case of keypress event - update the set of keys and call new fold ^
  switch sf cont
    where
      sf = proc input -> do
        mtx <- fromKeys mtx0 keys0 -< ()
        keyWp     <- key SDL.ScancodeW     "Pressed"  -< input
        keyWr     <- key SDL.ScancodeW     "Released" -< input
        keySp     <- key SDL.ScancodeS     "Pressed"  -< input
        keySr     <- key SDL.ScancodeS     "Released" -< input
        keyAp     <- key SDL.ScancodeA     "Pressed"  -< input
        keyAr     <- key SDL.ScancodeA     "Released" -< input
        keyDp     <- key SDL.ScancodeD     "Pressed"  -< input
        keyDr     <- key SDL.ScancodeD     "Released" -< input
                                                      
        keyQp     <- key SDL.ScancodeQ     "Pressed"  -< input
        keyQr     <- key SDL.ScancodeQ     "Released" -< input
        keyEp     <- key SDL.ScancodeE     "Pressed"  -< input
        keyEr     <- key SDL.ScancodeE     "Released" -< input
        keyZp     <- key SDL.ScancodeZ     "Pressed"  -< input
        keyZr     <- key SDL.ScancodeZ     "Released" -< input
        keyXp     <- key SDL.ScancodeX     "Pressed"  -< input
        keyXr     <- key SDL.ScancodeX     "Released" -< input
        
        keyUpP    <- key SDL.ScancodeUp    "Pressed"  -< input
        keyUpR    <- key SDL.ScancodeUp    "Released" -< input
        keyDownP  <- key SDL.ScancodeDown  "Pressed"  -< input
        keyDownR  <- key SDL.ScancodeDown  "Released" -< input
        keyLeftP  <- key SDL.ScancodeLeft  "Pressed"  -< input
        keyLeftR  <- key SDL.ScancodeLeft  "Released" -< input
        keyRightP <- key SDL.ScancodeRight "Pressed"  -< input
        keyRightR <- key SDL.ScancodeRight "Released" -< input
        
        let result = ( mtx
                  , Keys
                    ( keyEvent (keyW     keys0) keyWp      keyWr     )
                    ( keyEvent (keyS     keys0) keySp      keySr     )
                    ( keyEvent (keyA     keys0) keyAp      keyAr     )
                    ( keyEvent (keyD     keys0) keyDp      keyDr     )
                    ( keyEvent (keyQ     keys0) keyQp      keyQr     )
                    ( keyEvent (keyE     keys0) keyEp      keyEr     )
                    ( keyEvent (keyZ     keys0) keyZp      keyZr     )
                    ( keyEvent (keyX     keys0) keyXp      keyXr     )
                    ( keyEvent (keyUp    keys0) keyUpP     keyUpR    )
                    ( keyEvent (keyDown  keys0) keyDownP   keyDownR  )
                    ( keyEvent (keyLeft  keys0) keyLeftP   keyLeftR  )
                    ( keyEvent (keyRight keys0) keyRightP  keyRightR ) )

        returnA -< ( mtx
                   , mergeEvents
                     [ keyWp,     keyWr
                     , keySp,     keySr
                     , keyAp,     keyAr
                     , keyDp,     keyDr
                     , keyQp,     keyQr
                     , keyEp,     keyEr 
                     , keyZp,     keyZr 
                     , keyXp,     keyXr
                     , keyUpP,    keyUpR
                     , keyDownP,  keyDownR
                     , keyLeftP,  keyLeftR
                     , keyRightP, keyRightR ]
                     `tag` result) -- :: (M44 Double, Event (M44 Double))
                     --`tag` (DT.trace ("result: " ++ show result) $ DT.trace ("sukanah: " ++ show result)) result) -- :: (M44 Double, Event (M44 Double))
          
      cont (mtx, keys) = updateTransform mtx vel0 keys -- undefined --fromKeys mtx keys

keyEvent :: Bool -> Event () -> Event () -> Bool
keyEvent state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise = state

fromKeys :: M44 Double -> Keys -> SF () (M44 Double)
fromKeys mtx0 keys0 =
  proc () -> do
    let translate = --DT.trace ("Hello!\n") $
          foldr (+) (view translation mtx0 ) $
          zipWith (*^) ((\x -> if x then 1 else 0) . ($ keys0) <$>
                        [keyW, keyS, keyA, keyD, keyZ, keyX])
                       ([fVel, bVel, lVel, rVel, uVel, dVel])

        rotate    = view _m33 mtx0

        mtx       = mkTransformationMat rotate translate

    returnA -< mtx
        where fVel   = V3 ( 0)  ( 0) (-0.1) -- forwards  velocity
              bVel   = V3 ( 0)  ( 0) ( 0.1) -- backwards velocity
              lVel   = V3 (-0.1)( 0) ( 0)   -- left      velocity
              rVel   = V3 ( 0.1)( 0) ( 0)   -- right     velocity
              uVel   = V3 ( 0)( 0.1) ( 0)   -- right     velocity
              dVel   = V3 ( 0)(-0.1) ( 0)   -- right     velocity
              
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
                         , keyRight ] `tag` result)

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
                            , keyRight ] `tag` p) :: (Double, Event Double)
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
    let obj = Object 0.0 geo (identity::M44 Double) (V4 0 0 0 0) (Keys False False False False False False False False False False False False)
    let initGame = Game GamePlaying obj
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
  game            <- initGame
  window          <- openWindow "e1337" (resX, resY)
  resources       <- initBufferObjects game
  
  animate
    window
    resources
    (parseWinInput >>> ((mainGame game) &&& handleExit))
