module Object
  ( Object (..)
  ) where

import Linear.V4

import Keys
import Geometry
import Controllable

data Object =
     Object
     { scalar     :: Double
     , geometry   :: Geo
     , velocity   :: V4 Double
     , controller :: Controllable
     } deriving Show
