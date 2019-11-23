module Controllable
  ( Controllable (..)
  , Device (..)
  , Keyboard (..)
  , Mouse (..)
  , transform
  , ypr      
  , device
  , mouse
  , keyboard
  ) where

import Linear.Matrix
import Linear.V3
import Control.Lens hiding (transform)

import Keyboard
import Mouse

data Controllable
  =  Controllable
     { _debug      :: (Double, Double)
     , _transform  :: M44 Double
     , _ypr        :: V3 Double  -- yaw/pitch/roll
     , _device     :: Device
     } deriving Show

transform :: Lens' Controllable (M44 Double)
ypr       :: Lens' Controllable (V3 Double)
device    :: Lens' Controllable Device
transform = lens _transform (\controllable newTransform -> Controllable { _transform = newTransform })
ypr       = lens _ypr       (\controllable newYpr       -> Controllable { _ypr       = newYpr })
device    = lens _device    (\controllable newDevice    -> Controllable { _device    = newDevice })

data Device
  =  Device
     {
       _keyboard :: Keyboard 
     , _mouse    :: Mouse    
     } deriving Show

mouse    :: Lens' Device Mouse
keyboard :: Lens' Device Keyboard
mouse    = lens _mouse    (\device newMouse    -> Device { _mouse    = newMouse })
keyboard = lens _keyboard (\device newKeyboard -> Device { _keyboard = newKeyboard })
