{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Solver
  ( Solver (..)
--  , solve'
  , solve''
  , spinSolver'
  , spinSolver''
  -- , pivot
  -- , transform
  ) where

import Linear.Matrix
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Control.Lens hiding (transform)
import FRP.Yampa
import FRP.Yampa.Switches

import Object hiding (solve')
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

-- solve' :: Solver -> Object -> Object
-- solve' (Spin _ _) _ = undefined
-- solve' (Gravity _) _ = undefined

instance VectorSpace (V3 Double) Double where
  zeroVector                   = (V3 0 0 0)
  (*^) s (V3 x y z)            = (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3 x y z) (V3 k l m) = (V3 (x+k) (y+l) (z+m))
  dot    (V3 x y z) (V3 k l m) = (x*k) + (y*l) + (z*m)


solve'' :: Solver -> Object -> SF () Object
solve'' (Spin pv0 ypr0) obj0 =
--  proc () -> do
--    returnA -< obj0
  proc () -> do
    ypr' <- (ypr0 ^+^) ^<< integral -< ypr0 --ypr1*100 -- add angular velocity, ypr <- M44 / Quaternion
    let mtx = 
          mkTransformationMat
          rot
          tr
          where
            rot =
              (view _m33 (Object._transform obj0))
              !*! fromQuaternion (axisAngle (view _x (view _m33 (Object._transform obj0))) (view _x ypr')) -- yaw
              !*! fromQuaternion (axisAngle (view _y (view _m33 (Object._transform obj0))) (view _y ypr')) -- pitch
              -- !*! fromQuaternion (axisAngle (view _z (view _m33 (Object._transform obj0))) (view _z ypr')) -- roll
              !*! fromQuaternion (axisAngle (view _z (view _m33 (Object._transform obj0))) (100)) -- roll
            tr  = view translation (Object._transform obj0) --undefined
    returnA -< obj0 { Object._transform = mtx }

spinSolver' :: Controllable -> V3 Double -> SF () (Controllable)
spinSolver' ctl0@(Solver mtx0 ypr0) ypr1 =
  proc () -> do
    ypr' <- (ypr0 ^+^) ^<< integral -< (V3 0 0 1000) --ypr1*100 -- add angular velocity, ypr <- M44 / Quaternion
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
            tr  = view translation mtx0 --undefined
    returnA -< ctl0 { Controllable._transform = mtx
                    , Controllable._ypr = ypr' }

spinSolver'' :: Solver -> Object -> SF () (Object)
spinSolver'' ctl0@(Spin pv0 ypr0) obj0 =
  proc () -> do
    ypr' <- ((V3 0 0 9999) ^+^) ^<< integral -< (V3 0 0 1000) --ypr1*100 -- add angular velocity, ypr <- M44 / Quaternion
    --let mtx0 = (Linear.Matrix.identity::M44 Double) -- view transform obj0
    let mtx' =
          V4
          (V4 1 0 0 0)
          (V4 0 2 0 0)
          (V4 0 0 1 0)
          (V4 0 0 0 1)
    let mtx0 = Object._transform obj0
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
            tr  = view translation mtx0 --undefined
    returnA -< obj0 { Object._transform = mtx }
      
--solve'' (Gravity _) _ = undefined
