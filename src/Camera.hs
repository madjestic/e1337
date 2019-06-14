module Camera
  ( Camera (..)
  ) where

import Controllable
import Keys

data Camera =
     Camera
     {
       controller   :: Controllable
     } deriving Show
