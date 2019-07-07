module Camera
  ( Camera (..)
  , initCam
  ) where

import            Controllable
import            Linear.Matrix (M44, M33, identity)
import            Linear (V3(..))
import            Keyboard

initCam
  = Camera
  ( Controllable
    (identity :: M44 Double)
    (V3 0 0 0)
    (Controller (Keyboard keys0 kvs0) (Mouse Nothing Nothing (0,0) mvs0 )))
  where
    mvs0   = undefined
    keys0  = ( Keys False False False False False False False False False False False False )
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
       driver :: Controllable
     } deriving Show
