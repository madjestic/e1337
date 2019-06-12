module Camera
  ( Camera (..)
  ) where

import Linear.Matrix
--import Linear.V4

import Keys

data Camera =
     Camera
     {
       transform :: M44 Double
     , keys      :: Keys  
     } deriving Show
