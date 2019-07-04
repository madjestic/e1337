module Controllable
  ( Controllable (..)
  , Controller (..)
  , Keyboard (..)
  , Mouse (..)
  ) where

import Linear.Matrix
import Linear.V3

import Keys

data Controllable
  =  Controllable
     {
       transform  :: M44 Double
     , ypr        :: V3 Double  -- yaw/pitch/roll
     , controller :: Controller
     -- , keys      :: Keys
     -- , keyVecs   :: [V3 Double]
     } deriving Show


data Keyboard
  =  Keyboard
  { -- | Keyboard State
    keys    :: Keys
  , keyVecs :: [V3 Double]
  } deriving Show

data Mouse
  =  Mouse
  { -- | Mouse State
    lmb :: Maybe (Double, Double)
  --, mmb
  , rmb :: Maybe (Double, Double)
  , pos ::       (Double, Double)
  } deriving Show

data Controller
  =  Controller
     {
       keyboard :: Keyboard 
     , mouse    :: Mouse    
     } deriving Show
