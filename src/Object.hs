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
  , programs
  , descriptors
  , transform
  , solver
  , solvers
  ) where

import Linear.V4
import Linear.Matrix -- (M44, M33, identity, translation, fromQuaternion, (!*!), mkTransformationMat)
import Linear (V3(..))
import Linear.Quaternion
import Control.Lens hiding (transform)
import FRP.Yampa    hiding (identity)
import FRP.Yampa.Switches
import Graphics.Rendering.OpenGL (Program (..))

import LoadShaders
import Keyboard
import Material
import Descriptor
import Solvable

import Debug.Trace    as DT
  
--------------------------------------------------------------------------------
-- < Object > ------------------------------------------------------------------

data Object
  =  Object
     { 
       _descriptors :: [Descriptor] -- | Material is bound in Descriptor, but we also use this data for draw-call separation per material.
                -- data Descriptor =
                     -- Descriptor VertexArrayObject NumArrayIndices
     , _materials   :: [Material]   -- | hence [Material] is present on the Object level too, we use that value, instead of looking it up from respective VGeo.
     , _programs    :: [Program]    -- | Shader Programs
     , _transforms  :: [M44 Double]
     , _velocity    :: V3 Double
     , _avelocity   :: V3 Double    -- | Angular velocity
     , _mass        :: Double
     , _density     :: Double
     , _time        :: Double
     , _solvers     :: [Solver]
     } deriving Show

descriptors :: Lens' Object [Descriptor]
descriptors = lens _descriptors (\object newDescriptors -> Object { _descriptors = newDescriptors })

materials :: Lens' Object [Material]
materials = lens _materials (\object newMaterial -> Object { _materials = newMaterial })

programs :: Lens' Object [Program]
programs = lens _programs (\object newProgram -> Object { _programs = newProgram })

transform :: Lens' Object [M44 Double]
transform = lens _transforms (\object newTransforms -> Object { _transforms = newTransforms })

solvers :: Lens' Object [Solver]
solvers = lens _solvers (\object newSolvers -> Object { _solvers = newSolvers })

-- solver :: Lens' Object (Controllable)
-- solver = lens _solver (\object newSolver -> Object { _solver = newSolver })

-- TODO : take translation (pivot offset) into account
instance Solvable Object where
  solver :: Solver -> Object -> SF () (Object)
  solver (Rotate pv0 ypr0) obj0 = -- TODO: there are multiple transforms per object
    proc () -> do
      --ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
      ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
      _ <- DT.trace ("Object._transforms obj0 :" ++ show (Object._transforms obj0)) $ returnA -< ()
      let mtx0 = Object._transforms obj0
          mtx = undefined :: [M44 Double]
            -- mkTransformationMat
            -- rot
            -- tr
            -- where
            --   rot =
            --     (view _m33 mtx0)
            --     !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
            --     !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
            --     !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
            --   --tr  = DT.trace ("view translation mtx0: " ++ show (view (_w._xyz) mtx0)) $ view (_w._xyz) mtx0
            --   --tr  = V3 0.777 0 0
            --  tr  = view (_w._xyz) mtx0
      --returnA -< obj0 { Object._transforms = (DT.trace ("mtx :" ++ show mtx) $ mtx) }
      returnA -< obj0 { Object._transforms = mtx }

-- spin :: V3 Double -> V3 Double -> M44 Double -> M44 Double
-- spin pivot rot mtx = undefined

defaultObj :: Object
defaultObj =
  Object.Object
    []
    [defaultMat]
    []
    [(identity::M44 Double)]
    (V3 0 0 0)
    (V3 0 0 0)
    (1.0)
    (1.0)
    (0.0)
    []
