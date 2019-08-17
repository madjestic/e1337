module Object
  ( Object (..)
  , initObj
  ) where

import Linear.V4

import            Keyboard
import            Geometry
import            Controllable
import            Linear.Matrix (M44, M33, identity)
import            Linear (V3(..))

initObj 
  = Object
    0.0
    (Geo [] [] [])
    (V4 0 0 0 0)
    ( Controllable
      (0,0)
      (identity :: M44 Double)
      (V3 0 0 0)
      (Devices (Keyboard keys0 kvs0) (Mouse Nothing Nothing (0,0) [])) )
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

data Object =
     Object
     { scalar     :: Double
     , geometry   :: Geo
     , velocity   :: V4 Double
     , driver     :: Controllable
     } deriving Show
