{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
--  .d8888b.        d8888888b     d8888888888888 
-- d88P  Y88b      d888888888b   d8888888        
-- 888    888     d88P88888888b.d88888888        
-- 888           d88P 888888Y88888P8888888888    
-- 888  88888   d88P  888888 Y888P 888888        
-- 888    888  d88P   888888  Y8P  888888        
-- Y88b  d88P d8888888888888   "   888888        
--  "Y8888P88d88P     888888       8888888888888

module Game
  ( Game    (..)
  , Stage   (..) 
  , Options (..)
  , options
  , Game.name
  , Game.resx
  , Game.resy
  , objects
  , camera
  , mainGame
  , gameIntro
  , gamePlay
  , updateGame
  , handleExit
  , centerView
  , initGame
  ) where

import Foreign.C                              (CInt)
import Unsafe.Coerce
import Control.Monad                          (mzero)
import Data.Maybe                             (fromMaybe)
import Data.Functor              (($>))
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Control.Lens.TH
import FRP.Yampa --(SF, returnA, isEvent, (>>^), switch)
import SDL.Input.Keyboard.Codes as SDL

import Controllable
import Camera
import Object
import AppInput
import Utils
import Project
import Descriptor
import Material

data Stage =
     GameIntro
   | GamePlaying
   | GameFinished
   | GameMenu
   deriving Show

loadDelay = 2.0  :: Double -- make it into Game options value

data Game =
     Game
     {
       _debug    :: (Double, Double)
     , _options  :: Options
     , _gStg     :: Stage
     , _objects  :: [Object]
     , _fonts    :: [Object] 
     , _camera   :: Camera
     } deriving Show

data Options
   = Options
   { _name  :: String
   , _resx  :: CInt
   , _resy  :: CInt
   } deriving Show

$(makeLenses ''Game)
$(makeLenses ''Options)

-- < Game Logic > ---------------------------------------------------------

mainGame :: Game -> SF AppInput Game
mainGame game0 =
  loopPre game0 $ 
  proc (input, game) -> do
    gs <- case _gStg game of
            GameIntro   -> gameIntro   -< (input, game)
            GamePlaying -> gamePlay game0 -< input
    returnA -< (gs, gs)

gameIntro :: SF (AppInput, Game) Game
gameIntro =
  switch sf cont        
     where sf =
             proc (input, game) -> do
               introState <- returnA -< game
               playState  <- returnA -< game { _gStg =  GamePlaying }
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after loadDelay () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> playState)
           cont game  = 
             proc input -> returnA -< game

gamePlay :: Game -> SF AppInput Game
gamePlay game =
  switch sf (const (mainGame game)) --cont       
     where sf =
             proc input -> do
               game'   <- updateGame game -< input
               reset   <- keyInput SDL.ScancodeSpace "Pressed" -< input
               returnA -< (game', reset $> game')
           --cont = const mainGame game

updateGame :: Game -> SF AppInput Game
updateGame game = 
  proc input -> do
    cam  <- updateCamera  $ Game._camera  game -< input
    objs <- updateObjects $ _objects game -< ()
    --returnA  -< game { Game._objects = (DT.trace ("updateGame.objs :" ++ show objs)$ objs)
    returnA  -< game { Game._objects = objs
                     , Game._camera  = cam }
    
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

centerView :: SF AppInput Bool
centerView = centerEvent >>^ isEvent

-- -- < Init Game State > ------------------------------------------------------

loadFonts :: IO [Object]
loadFonts =
  do
    return undefined

-- TODO: initGame could be loaded from a file, similar to "Save Game" feature.
initGame ::
     (([Int], Int, [Float], Material) -> IO Descriptor)
  -> (Project -> IO ())
  -> Project
  -> IO Game
initGame initVAO initGlobalUniforms project =
  do
    --_ <- DT.trace ("initializing game resources...") $ return ()
    print "initializing game resources..."
    _ <- initGlobalUniforms project
    objs  <- (loadObjects initVAO project)
    fonts <- loadFonts
    let camPos = fromList $ view cameraP project
    --pc <- fromVGeo $ fromPGeo pCloud
    --let objs = [pc]
    let game =
          Game
          (-42,-17)
          ( Options
            name'
            resX'
            resY'
          )
          GamePlaying
          --(DT.trace ("initGame.objs :" ++ show objs) $ objs)
          objs
          fonts
          --initCam -- TODO: read campos from project file
          (Camera  initCamController { Controllable._transform = camPos })
    return game
      where        
        name' = Project.name  $ project
        resX' = (unsafeCoerce $ Project.resx $ project) :: CInt
        resY' = (unsafeCoerce $ Project.resy $ project) :: CInt
