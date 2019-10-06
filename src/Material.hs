module Material
  ( Material (..)
  , defaultMat
  ) where

defaultMat
  = Material
    "shaders/shader.vert"
    "shaders/shader.frag"
    []
  

data Material
  =  Material
     {
       vertShader :: FilePath   -- path to vertex shader program
     , fragShader :: FilePath   -- path to fragment shader program
     , textures   :: [FilePath] -- paths to texture bindings
     } deriving Show
