module Drawables
  ( Drawables(..)
  , Drawable(..)
  ) where

import Graphics.Rendering.OpenGL as GL        ( Vertex4(..)
                                              , TexCoord3(..)
                                              , GLuint
                                              , GLfloat )


data Drawable
  =  Drawable
    { verts  :: [GLfloat]
    , ids    :: [GLuint]
    } deriving Show

class Drawables a where
  toDrawable :: a -> IO Drawable
