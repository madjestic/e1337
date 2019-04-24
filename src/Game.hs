--  .d8888b.        d8888888b     d8888888888888 
-- d88P  Y88b      d888888888b   d8888888        
-- 888    888     d88P88888888b.d88888888        
-- 888           d88P 888888Y88888P8888888888    
-- 888  88888   d88P  888888 Y888P 888888        
-- 888    888  d88P   888888  Y8P  888888        
-- Y88b  d88P d8888888888888   "   888888        
--  "Y8888P88d88P     888888       8888888888888 

module Game
  ( Game  (..)
  , Stage (..)
  , Object(..)
  , Keys  (..)
  ) where

import Geometry
import Linear.Matrix
import Linear.V4

-- meta game state
data Stage =
     GameIntro
   | GamePlaying
   | GameFinished
   | GameMenu
   deriving Show

data Keys =
     Keys
     { keyW  :: Bool
     , keyS  :: Bool
     , keyA  :: Bool
     , keyD  :: Bool
     , keyZ  :: Bool
     , keyX  :: Bool
     , keyQ  :: Bool
     , keyE  :: Bool
     , keyUp    :: Bool
     , keyDown  :: Bool
     , keyLeft  :: Bool
     , keyRight :: Bool
     } deriving Show

data Object =
     Object
     { scalar    :: Double
     , geometry  :: Geo
     , transform :: M44 Double
     , velocity  :: V4 Double
     , keys      :: Keys
     } deriving Show

-- game state
data Game =
     Game
     { gStg     :: Stage
     , object   :: Object
     } deriving Show
