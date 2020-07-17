{-# LANGUAGE TemplateHaskell #-}

module Project 
  ( Project  (..)
  , Model    (..)
  , name
  , resx
  , resy
  , models
  , cameraP
  ) where

import Texture
import Model

import Control.Lens
import Control.Lens.TH

data Project
  =  Project
     {
       name      :: String
     , resx      :: Int
     , resy      :: Int
     , _models   :: [Model]
     , _textures :: [Texture]
     , _cameraP  :: [Float]
     } deriving Show

$(makeLenses ''Project)
