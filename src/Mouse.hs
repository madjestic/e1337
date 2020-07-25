{-# LANGUAGE TemplateHaskell, Arrows #-}

module Mouse
  ( Mouse (..)
  , pos
  , mmov
  ) where

import Control.Lens
import Control.Lens.TH

import Linear.V3
import FRP.Yampa (SF, Event)

import AppInput

data Mouse
  =  Mouse
  { -- | Mouse State
    lmb   :: Maybe (Double, Double)
  --, mmb
  , rmb   :: Maybe (Double, Double)
  , _pos  ::       (Double, Double)
  , _rpos ::       (Double, Double)
  , _mmov ::        Bool
  , mVecs ::       [V3 Double]
  } deriving Show

$(makeLenses ''Mouse)

