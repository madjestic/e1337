-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Object
  ( Object (..)
  , defaultObj
--  , scalar
  , materials
  , descriptors
  , transform
  , solver
  ) where

import Linear.V4
import Linear.Matrix -- (M44, M33, identity, translation, fromQuaternion, (!*!), mkTransformationMat)
import Linear (V3(..))
import Linear.Quaternion
import Control.Lens hiding (transform)
import FRP.Yampa    hiding (identity)
import FRP.Yampa.Switches

--import Controllable hiding (_transform, transform)
import Keyboard
import Material
import Descriptor
import Solvable
  
--------------------------------------------------------------------------------
-- < Object > ------------------------------------------------------------------

data Object
  =  Object
     { 
       _descriptors :: [Descriptor]
     , _materials   :: [Material]
     , _transform   :: M44 Double -- this is the problem, TODO: possibly remove the _solver
     , _velocity    :: V4 Double
--     , _solver      :: [Solver] -- TODO:introduce Solver stack
     } deriving Show

descriptors :: Lens' Object [Descriptor]
descriptors = lens _descriptors (\object newDescriptors -> Object { _descriptors = newDescriptors })

materials :: Lens' Object [Material]
materials = lens _materials (\object newMaterial -> Object { _materials = newMaterial })

transform :: Lens' Object (M44 Double)
transform = lens _transform (\object newTransform -> Object { _transform = newTransform })

-- solver :: Lens' Object (Controllable)
-- solver = lens _solver (\object newSolver -> Object { _solver = newSolver })

instance Solvable Object where
  solver :: Solver -> Object -> SF () (Object)
  solver (Spin pv0 ypr0) obj0 =
    proc () -> do
      ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
      let mtx0 = Object._transform obj0
          mtx = 
            mkTransformationMat
            rot
            tr
            where
              rot =
                (view _m33 mtx0)
                !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
                !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
                !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
              tr  = view translation mtx0
      returnA -< obj0 { Object._transform = mtx }

defaultObj :: Object
defaultObj =
  Object.Object
    []
    [defaultMat]
    (identity::M44 Double)
    (V4 0 0 0 0)
    -- ( Solver
    --   (identity :: M44 Double)
    --   (V3 (0) (0) (-10.1)))
