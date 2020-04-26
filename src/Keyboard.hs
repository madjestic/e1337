module Keyboard
  ( Keyboard (..)
  , Keys (..)
  ) where

import Linear.V3

data Keyboard
  =  Keyboard
  { -- | Keyboard State
    keys    :: Keys
  , keyVecs :: [V3 Double]
  } deriving Show

data Keys =
     Keys
     { keyW      :: Bool
     , keyS      :: Bool
     , keyA      :: Bool
     , keyD      :: Bool
     , keyQ      :: Bool
     , keyE      :: Bool
     , keyZ      :: Bool
     , keyC      :: Bool
     , keyUp     :: Bool
     , keyDown   :: Bool
     , keyLeft   :: Bool
     , keyRight  :: Bool
     , keyPageUp :: Bool
     , keyPageDown :: Bool
     , keyLShift :: Bool
     , keyLCtrl  :: Bool
     } deriving Show
