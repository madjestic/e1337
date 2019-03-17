module Drawables
  ( Drawables(..)
  , Drawable(..)
  ) where

import Graphics.Rendering.OpenGL as GL        (Vertex4(..),TexCoord2(..))


data Drawable
  =
    Drawable
    {
      verts  :: [Vertex4   Double]
    , uvs    :: [TexCoord2 Double]
    , ids    :: [Int]
    }

class Drawables a where
  toDrawable :: a -> IO Drawable
