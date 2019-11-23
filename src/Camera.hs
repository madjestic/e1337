module Camera
  ( Camera (..)
  , initCam
  , controller
  ) where

import Control.Lens
import Linear.Matrix             (M44, M33, identity)
import Linear                    (V3(..))
import Graphics.Rendering.OpenGL (GLmatrix, GLfloat)

import Controllable
import Keyboard

initCam
  = Camera
  ( Controllable
    (0,0)
    (identity :: M44 Double)
    (V3 0 0 0)
    (Device (Keyboard keys0 kvs0) (Mouse Nothing Nothing (0,0) mvs0 )))
  where
    mvs0   = [] --undefined
    -- mvs0 - mouse vectors
    keys0  = ( Keys False False False False False False False False False False False False )
    -- kvs0 - key vectors
    kvs0   = [ fVel, bVel, lVel, rVel, uVel, dVel, pPitch, nPitch, pYaw, nYaw, pRoll, nRoll ]
    fVel   = V3 ( 0  )( 0  )( 0.1)   -- forwards  velocity
    bVel   = V3 ( 0  )( 0  )(-0.1)   -- backwards velocity
    lVel   = V3 ( 0.1)( 0  )( 0  )   -- left      velocity
    rVel   = V3 (-0.1)( 0  )( 0  )   -- right     velocity
    uVel   = V3 ( 0  )(-0.1)( 0  )   -- right     velocity
    dVel   = V3 ( 0  )( 0.1)( 0  )   -- right     velocity
    pPitch = V3 ( 0.1)( 0  )( 0  )   -- positive  pitch
    nPitch = V3 (-0.1)( 0  )( 0  )   -- negative  pitch
    pYaw   = V3 ( 0  )(-0.1)( 0  )   -- positive  yaw
    nYaw   = V3 ( 0  )( 0.1)( 0  )   -- negative  yaw
    pRoll  = V3 ( 0  )(  0 )(-0.1)   -- positive  roll
    nRoll  = V3 ( 0  )(  0 )( 0.1)   -- negative  roll


data Camera =
     Camera
     {
       _controller :: Controllable
     } deriving Show

controller :: Lens' Camera Controllable
controller = lens _controller (\camera newController -> Camera { _controller = newController })
