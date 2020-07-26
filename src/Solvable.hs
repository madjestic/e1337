{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module Solvable
  ( Solver (..)
  , Solvable (..)
  , spin
  ) where

import Linear.Matrix
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Control.Lens hiding (transform)
import FRP.Yampa

import Utils ()

data Solver =
     Translate
     {
       _txyz   :: V3 Double
     }
  |  Rotate
     {
       _pivot :: V3 Double
     , _ypr   :: V3 Double
     }
  |  Scale
    {
      _sxyz   :: V3 Double
    }
  -- |  Gravity
  --   {
  --     _G :: Double
  --   }
  deriving Show

class Solvable a where
  solver :: Solver -> a -> SF () a

spin :: V3 Double -> V3 Double -> M44 Double -> SF () (M44 Double)
spin pv0 ypr0 mtx0 =
  proc () -> do
    ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
    let mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                (view _m33 mtx0)
                !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
                !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
                !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
              tr  = view (_w._xyz) mtx0

    returnA -< mtx
