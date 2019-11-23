module Mouse
  ( Mouse (..)
  , pos
  ) where

import Control.Lens
import Linear.V3

data Mouse
  =  Mouse
  { -- | Mouse State
    lmb :: Maybe (Double, Double)
  --, mmb
  , rmb :: Maybe (Double, Double)
  , _pos ::       (Double, Double)
  , mvecs :: [V3 Double]
  } deriving Show

pos :: Lens' Mouse (Double, Double)
pos = lens _pos (\mouse newPos -> Mouse { _pos = newPos })
