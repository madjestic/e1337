module PCloud where

import Linear.Matrix
import Linear.V4
import Unsafe.Coerce

import PGeo

pCloud :: PGeo
pCloud =
  PGeo
  {
    ids   = [take (length ps') [0..]]
  , as    = take (length ps') $ repeat (1.0)
  , cs    = take (length ps') $ repeat (1.0, 0.0, 0.0) :: [Vec3]
  , ns    = take (length ps') $ repeat (0.0, 0.0, 1.0) :: [Vec3]
  , uvws  = ps'
  , ps    = ps'
  , mats  = ["mat/test/test"]
  , xform = [toList (identity :: M44 Double)]
  }
  where
    ps' = [(x,y,0.0) | x <- [0.0, 0.1 .. 1.0], y <- [0.0, 0.1 .. 1.0]] :: [Vec3]

toList :: M44 Double -> [Float]
toList (V4 (V4 x y z w) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) (V4 x3 y3 z3 w3))
  = fmap unsafeCoerce
    [ x ,y ,z ,w
    , x1,y1,z1,w1
    , x2,y2,z2,w2
    , x3,y3,z3,w3 ] :: [Float]
