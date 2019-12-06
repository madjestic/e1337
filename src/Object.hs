{-# LANGUAGE OverloadedStrings #-}

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
import Linear.Matrix (M44, M33, identity)
import Linear (V3(..))
import Control.Lens hiding (transform)

import Controllable hiding (_transform, transform)
-- import Geometry
import Keyboard
import Material
import Descriptor
  
--------------------------------------------------------------------------------
-- < Object > ------------------------------------------------------------------

data Object
  =  Object
     { --_scalar      :: Double
       _descriptors :: [Descriptor]
     , _materials   :: [Material]
     , _transform   :: M44 Double -- this is the problem, TODO: possibly remove the _solver
     , _velocity    :: V4 Double
     , _solver      :: Controllable
     } deriving Show

-- scalar :: Lens' Object Double
-- scalar = lens _scalar (\object newScalar -> Object { _scalar = newScalar })

descriptors :: Lens' Object [Descriptor]
descriptors = lens _descriptors (\object newDescriptors -> Object { _descriptors = newDescriptors })

materials :: Lens' Object [Material]
materials = lens _materials (\object newMaterial -> Object { _materials = newMaterial })

transform :: Lens' Object (M44 Double)
transform = lens _transform (\object newTransform -> Object { _transform = newTransform })

solver :: Lens' Object (Controllable)
solver = lens _solver (\object newSolver -> Object { _solver = newSolver })

defaultObj 
  = Object.Object
    []
    [defaultMat]
    (identity::M44 Double)
    (V4 0 0 0 0)
    ( Solver
      (identity :: M44 Double)
      (V3 (0) (0) (-10.1)))
