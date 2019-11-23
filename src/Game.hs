{-# LANGUAGE TemplateHaskell #-}
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
  , name
  , resx
  , resy
  , objects
  , camera
  ) where

import Foreign.C                              (CInt)
import Unsafe.Coerce
import Control.Monad                          (mzero)
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Control.Lens

import Camera
import Object

data Game =
     Game
     {
       _options  :: Options
     , _gStg     :: Stage
     , _objects  :: [Object]
     , _camera   :: Camera
     } deriving Show

data Options
   = Options
   { _name  :: String
   , _resx  :: CInt
   , _resy  :: CInt
   } deriving Show

data Stage =
     GameIntro
   | GamePlaying
   | GameFinished
   | GameMenu
   deriving Show

options :: Lens' Game    Options
name    :: Lens' Options String
resx    :: Lens' Options CInt
resy    :: Lens' Options CInt
objects :: Lens' Game    [Object]
camera  :: Lens' Game    Camera
options = lens _options (\game newOptions -> Game    { _options = newOptions })
name    = lens _name    (\options newName -> Options { _name    = newName })
resx    = lens _resx    (\options newX    -> Options { _resx    = newX })
resy    = lens _resy    (\options newY    -> Options { _resy    = newY })
objects = lens _objects (\game newObjects -> Game    { _objects = newObjects})
camera  = lens _camera  (\game newCamera  -> Game    { _camera  = newCamera })

