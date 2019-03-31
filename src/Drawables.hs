module Drawables
  ( Drawables(..)
  , Drawable(..)
  ) where

import Graphics.Rendering.OpenGL as GL        (Vertex4(..)
                                              ,TexCoord3(..)
                                              ,GLuint)


data Drawable
  =
    Drawable
    {
      verts  :: [Vertex4 Double]
    , uvs    :: [TexCoord3 Double]
    , ids    :: [GLuint]
    } deriving Show

class Drawables a where
  toDrawable :: a -> IO Drawable