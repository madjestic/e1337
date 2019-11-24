{-# LANGUAGE OverloadedStrings #-}

module Object
  ( Object (..)
  , defaultObj
--  , scalar
  , materials
  , descriptors
  , transform
  ) where

-- import Control.Monad             (mzero)
-- import Data.Aeson           as A
-- import Data.ByteString.Lazy as B
-- import Data.Maybe                (fromMaybe)

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
     , _transform   :: M44 Double
     , _velocity    :: V4 Double
     , _driver      :: Controllable
     } deriving Show

-- scalar :: Lens' Object Double
-- scalar = lens _scalar (\object newScalar -> Object { _scalar = newScalar })

descriptors :: Lens' Object [Descriptor]
descriptors = lens _descriptors (\object newDescriptors -> Object { _descriptors = newDescriptors })

materials :: Lens' Object [Material]
materials = lens _materials (\object newMaterial -> Object { _materials = newMaterial })

transform :: Lens' Object (M44 Double)
transform = lens _transform (\object newTransform -> Object { _transform = newTransform })

defaultObj 
  = Object.Object
    -- 0.0
    []                        --"models/square.pgeo" --(Geo [[]] [] [] [] [] [])
    [defaultMat]
    (identity::M44 Double)
    (V4 0 0 0 0)
    ( Controllable
      (0,0)
      (identity :: M44 Double)
      (V3 0 0 0)
      (Device (Keyboard keys0 kvs0) (Mouse Nothing Nothing (0,0) [])) )
          where
            keys0  = ( Keys False False False False False False False False False False False False )
            kvs0   = [ fVel, bVel, lVel, rVel, uVel, dVel, pPitch, nPitch, pYaw, nYaw, pRoll, nRoll ]
            fVel   = V3 ( 0  )( 0  )( 999)   -- forwards  velocity
            bVel   = V3 ( 0  )( 0  )(-999)   -- backwards velocity
            lVel   = V3 ( 999)( 0  )( 0  )   -- left      velocity
            rVel   = V3 (-999)( 0  )( 0  )   -- right     velocity
            uVel   = V3 ( 0  )(-999)( 0  )   -- right     velocity
            dVel   = V3 ( 0  )( 999)( 0  )   -- right     velocity
            pPitch = V3 ( 999)( 0  )( 0  )   -- positive  pitch
            nPitch = V3 (-999)( 0  )( 0  )   -- negative  pitch
            pYaw   = V3 ( 0  )( 999)( 0  )   -- positive  yaw
            nYaw   = V3 ( 0  )(-999)( 0  )   -- negative  yaw
            pRoll  = V3 ( 0  )(  0 )( 999)   -- positive  roll
            nRoll  = V3 ( 0  )(  0 )(-999)   -- negative  roll
