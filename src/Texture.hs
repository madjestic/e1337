{-# LANGUAGE TemplateHaskell #-}

module Texture 
  ( Texture (..)
  , path
  ) where

import Control.Lens
import Control.Lens.TH

data Texture
  =  Texture
     {
       _path :: String
     } deriving Show

$(makeLenses ''Texture)
