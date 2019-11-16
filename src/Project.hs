module Project 
  ( Project  (..)
  , Model    (..)
--  , Material (..)
  , name
  , resx
  , resy
  , models
  ) where

--import Material
import Control.Lens

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

-- name :: Lens' Project String
-- name = lens _name (\project  newName -> Project { _name = newName })

-- resx :: Lens' Project Int
-- resx = lens _resx (\project newX -> Project { _resx = newX })

-- resy :: Lens' Project Int
-- resy = lens _resy (\project newY -> Project { _resy = newY })

-- models :: Lens' Project [Model]
-- models = lens _models (\project newModels -> Project { _models = newModels })

