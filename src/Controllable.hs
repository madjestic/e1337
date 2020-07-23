{-# LANGUAGE TemplateHaskell #-}

module Controllable
  ( Controllable (..)
  , Device (..)
  , Keyboard (..)
  , Mouse (..)
  , transform
  , transform'
  , ypr      
  , device
  , device'
  , mouse
  , keyboard
  ) where

import Linear.Matrix
import Linear.V3
import Control.Lens hiding (transform)
import Control.Lens.TH

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
--       _pivot      :: V3 Double
       _transform  :: M44 Double
     , _ypr        :: V3 Double  -- yaw/pitch/roll
--     , _velocity   :: V3 Double
--     , _physC      :: Physics -- TODO : add phys.parms
     }
  deriving Show

data Device
  =  Device
     {
       _keyboard :: Keyboard 
     , _mouse    :: Mouse    
     } deriving Show

transform' :: Lens' Controllable (M44 Double)
transform' = lens _transform (\controllable newTransform -> Solver { _transform = newTransform })

device'    :: Lens' Controllable Device
device'    = lens _device    (\controllable newDevice    -> Controller { _device    = newDevice })

$(makeLenses ''Device)
$(makeLenses ''Controllable)
