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
  ) where

import Camera
import Geometry
import Object
import Foreign.C                              (CInt)
                                              
import Control.Monad                          (mzero)
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as B

-- meta game state
data Stage =
     GameIntro
   | GamePlaying
   | GameFinished
   | GameMenu
   deriving Show

data Options
   = Options
   { lable :: String
   , resolution :: (CInt, CInt)
   } deriving Show

-- game state
data Game =
     Game
     {
       options  :: Options
     , gStg     :: Stage
     , object   :: Object
     , camera   :: Camera
     } deriving Show
