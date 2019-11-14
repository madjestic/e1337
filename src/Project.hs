module Project 
  ( Project  (..)
  , Model    (..)
--  , Material (..)
  ) where

import Material

data Project
  =  Project
     {
       name   :: String
     , resx   :: Int
     , resy   :: Int
     , models :: [Model]
     } deriving Show

data Model
  =  Model
     {
       path      :: String
     -- , materials :: [Material]
     } deriving Show
