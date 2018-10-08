module Types where

import FRP.Yampa

import qualified SDL

type WinInput = Event SDL.EventPayload
type Vec2     = (Double, Double)
type Vec3     = (Double, Double, Double)

data PGeo =
     PGeo
     {
       pgeo_positions :: [Vec3] -- position of vertices positions as Vec3
     , uv             :: [Vec3]
     } deriving Show
