{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Camera
  ( Camera (..)
  , initCam
  , controller
  , initCamController
  , updateCamera
  ) where

import Control.Lens
import Linear.Matrix             (M44, M33, identity, transpose)
import Linear                    (V3(..), V4 (..))
import Graphics.Rendering.OpenGL (GLmatrix, GLfloat)
import FRP.Yampa (SF, returnA)

import Controllable
import Keyboard
import AppInput

data Camera =
     Camera
     {
       _controller :: Controllable
     } deriving Show

$(makeLenses ''Camera)

initCam :: Camera
initCam
  = Camera initCamController

initCamController :: Controllable
initCamController =
  ( Controller
    (0,0)
    -- (transpose (identity :: M44 Double))
    (
      (V4
        (V4 1 0 0 0)
        (V4 0 1 0 0) -- <- . . . y ...
        (V4 0 0 1 (-60)) -- <- . . . z-component of transform
        (V4 0 0 0 1)))
    (V3 0 0 0) -- rotation
    (Device (Keyboard keys0 kvs0) (Mouse Nothing Nothing (0,0) (0.0, 0.0) False mvs0 )))
  where
    mvs0   = [] --undefined
    -- mvs0 - mouse vectors
    keys0  = ( Keys False False False False False False False False False False False False False False False False )
    -- kvs0 - key vectors
    kvs0   = [ fVel, bVel, lVel, rVel, uVel, dVel, pPitch, nPitch, pYaw, nYaw, pRoll, nRoll ]
    fVel   = V3 ( 0  )( 0  )( 0.1)   -- forwards  velocity
    bVel   = V3 ( 0  )( 0  )(-0.1)   -- backwards velocity
    lVel   = V3 ( 0.1)( 0  )( 0  )   -- left      velocity
    rVel   = V3 (-0.1)( 0  )( 0  )   -- right     velocity
    uVel   = V3 ( 0  )(-0.1)( 0  )   -- right     velocity
    dVel   = V3 ( 0  )( 0.1)( 0  )   -- right     velocity
    pPitch = V3 (-1.0)( 0  )( 0  )   -- positive  pitch
    nPitch = V3 ( 1.0)( 0  )( 0  )   -- negative  pitch
    pYaw   = V3 ( 0  )(-1.0)( 0  )   -- positive  yaw
    nYaw   = V3 ( 0  )( 1.0)( 0  )   -- negative  yaw
    pRoll  = V3 ( 0  )(  0 )(-1.0)   -- positive  roll
    nRoll  = V3 ( 0  )(  0 )( 1.0)   -- negative  roll

-- controller :: Lens' Camera Controllable
-- controller = lens _controller (\camera newController -> Camera { _controller = newController })

updateCamera :: Camera -> SF AppInput Camera
updateCamera cam0 = 
  proc input ->
    do
      ctl' <- updateController (view controller cam0) -< input
      let
        cam' = cam0 { Camera._controller = ctl' }
      returnA -< cam'
