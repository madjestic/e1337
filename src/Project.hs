module Project 
  ( Project  (..)
  , Model    (..)
--  , Material (..)
  , name
  , resx
  , resy
  , models
  , cameraP
  , path
  ) where

--import Material
import Control.Lens

data Project
  =  Project
     {
       name    :: String
     , resx    :: Int
     , resy    :: Int
     , _models :: [Model]
     , _cameraP :: [Float]
     } deriving Show

data Model
  =  Model
     {
       _path      :: String
     } deriving Show

-- name :: Lens' Project String
-- name = lens _name (\project  newName -> Project { _name = newName })

-- resx :: Lens' Project Int
-- resx = lens _resx (\project newX -> Project { _resx = newX })

-- resy :: Lens' Project Int
-- resy = lens _resy (\project newY -> Project { _resy = newY })

models :: Lens' Project [Model]
models = lens _models (\project newModels -> Project { _models = newModels })

cameraP :: Lens' Project [Float]
cameraP = lens _cameraP (\project newCameraP -> Project { _cameraP = newCameraP })
-- TOODO : finish lensing:..
path :: Lens' Model String
path = lens _path (\model newPath -> Model { _path = newPath })
