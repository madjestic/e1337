module Object
  ( Object (..)
  ) where

import Linear.Matrix
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
     -- , transform :: M44 Double
     -- , keys      :: Keys
--     , angular   :: V4 Double -- angular velocity
     } deriving Show
