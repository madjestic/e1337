module Mouse
  ( Mouse (..)
  ) where

import Linear.V3

data Mouse
  =  Mouse
  { -- | Mouse State
    lmb :: Maybe (Double, Double)
  --, mmb
  , rmb :: Maybe (Double, Double)
  , pos ::       (Double, Double)
  , mvecs :: [V3 Double]
  } deriving Show
