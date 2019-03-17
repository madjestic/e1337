{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Shape2D
  ( square
  , toUV
  , Shape2D(..)
  , FromVector(..)
  ) where

import Graphics.Rendering.OpenGL as GL        (Vertex4(..),TexCoord2(..))

import FromVector


-- < 2D Shapes > -----------------------------------------------------------
type Vec2     = (Double, Double)
type Size     = Double
  
data Shape2D
   = Square Vec2 Size
   deriving Show

instance FromVector Vec2 where
  toVertex4 :: Vec2 -> Vertex4 Double
  toVertex4 (k, l)    = Vertex4 k l 0.0 1.0

square :: Vec2 -> Double -> [Vec2]
square pos side = [p1, p2, p3,
                   p1, p3, p4]
    where          
        x = fst pos
        y = snd pos
        r = side/2 
        p1 = (x + r, y + r)
        p2 = (x - r, y + r)
        p3 = (x - r, y - r)
        p4 = (x + r, y - r)

-- texturing ---------------------------------------------------------------------
data Projection
   = Planar
   deriving Show

toUV :: Projection -> [TexCoord2 Double]
toUV Planar =
  projectPlanar ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)] :: [Vec2]

projectPlanar :: [Vec2] -> [TexCoord2 Double]
projectPlanar      = map $ uncurry TexCoord2
                                                                  
