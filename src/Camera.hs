module Camera
  ( Camera (..)
  ) where

import Linear.Matrix
import Linear.V3

import Keys

data Camera =
     Camera
     {
       transform :: M44 Double
     , ypr       :: V3 Double  -- yaw/pitch/roll
     , keys      :: Keys  
     } deriving Show
