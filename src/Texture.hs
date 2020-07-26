{-# LANGUAGE TemplateHaskell #-}

module Texture 
  ( Texture (..)
  , path
  ) where

import Control.Lens

data Texture
  =  Texture
     {
       _path :: String
     } deriving Show

$(makeLenses ''Texture)
