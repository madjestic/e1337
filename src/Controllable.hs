module Controllable
  ( Controllable (..)
  , Controller (..)
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
     , controller :: Controller
     } deriving Show

data Controller
  =  Controller
     {
       keyboard :: Keyboard 
     , mouse    :: Mouse    
     } deriving Show
