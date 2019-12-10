{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Solvable
  ( Solver (..)
  , Solvable (..)
--  , solver
  ) where

import Linear.Matrix
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Control.Lens hiding (transform)
import FRP.Yampa
import FRP.Yampa.Switches

--import Object hiding (solve')
import Controllable (Controllable (Solver),  _transform, _ypr)

data Solver
  =  Spin
     {
       _pivot :: V3 Double
     , _ypr   :: V3 Double
--     , _formula :: (V3 Double) -> Solver
     }
  -- |  Gravity
  --   {
  --     _G :: Double
  --   }
  deriving Show

instance VectorSpace (V3 Double) Double where
  zeroVector                   = (V3 0 0 0)
  (*^) s (V3 x y z)            = (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3 x y z) (V3 k l m) = (V3 (x+k) (y+l) (z+m))
  dot    (V3 x y z) (V3 k l m) = (x*k) + (y*l) + (z*m)

class Solvable a where
  solver :: Solver -> a -> SF () a
