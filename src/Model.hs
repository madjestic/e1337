{-# LANGUAGE TemplateHaskell #-}

module Model 
  ( Model (..)
  , path
  ) where

import Control.Lens

data Model
  =  Model
     {
       _path :: String
     } deriving Show

$(makeLenses ''Model)
