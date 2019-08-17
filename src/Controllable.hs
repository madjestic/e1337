module Controllable
  ( Controllable (..)
  , Devices (..)
  , Keyboard (..)
  , Mouse (..)
  ) where

import Linear.Matrix
import Linear.V3

import Keyboard
import Mouse

data Controllable
  =  Controllable
     { debug      :: (Double, Double)
     , transform  :: M44 Double
     , ypr        :: V3 Double  -- yaw/pitch/roll
     , devices    :: Devices
     } deriving Show

data Devices
  =  Devices
     {
       keyboard :: Keyboard 
     , mouse    :: Mouse    
     } deriving Show
