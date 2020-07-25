{-# LANGUAGE TemplateHaskell #-}

module Model 
  ( Model (..)
  , path
  ) where

import Control.Lens
import Control.Lens.TH

data Model
  =  Model
     {
       _path :: String
     } deriving Show

$(makeLenses ''Model)
