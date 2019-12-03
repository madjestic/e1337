module Controllable
  ( Controllable (..)
  , Device (..)
  , Keyboard (..)
  , Mouse (..)
  , transform
  , transform'
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
  =  Controller
     {
       _debug      :: (Double, Double)
     , _transform  :: M44 Double
     , _ypr        :: V3 Double  -- yaw/pitch/roll
     , _device     :: Device
     }
  |  Solver
     {
       _transform  :: M44 Double
     , _ypr        :: V3 Double  -- yaw/pitch/roll
     }
  deriving Show

transform :: Lens' Controllable (M44 Double)
transform = lens _transform (\controllable newTransform -> Controller { _transform = newTransform })

transform' :: Lens' Controllable (M44 Double)
transform' = lens _transform (\controllable newTransform -> Solver { _transform = newTransform })

ypr       :: Lens' Controllable (V3 Double)
ypr       = lens _ypr       (\controllable newYpr       -> Controller { _ypr       = newYpr })

device    :: Lens' Controllable Device
device    = lens _device    (\controllable newDevice    -> Controller { _device    = newDevice })

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
