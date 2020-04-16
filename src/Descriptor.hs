module Descriptor
  (Descriptor (..)) where

import Graphics.Rendering.OpenGL (VertexArrayObject, NumArrayIndices)

data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices
  deriving Show
