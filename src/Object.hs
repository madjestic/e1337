module Object
  ( Object (..)
  ) where

import Linear.Matrix
import Linear.V4

import Keys
import Geometry

data Object =
     Object
     { scalar    :: Double
     , geometry  :: Geo
     , transform :: M44 Double
     , velocity  :: V4 Double
--     , angular   :: V4 Double -- angular velocity
     , keys      :: Keys
     } deriving Show
