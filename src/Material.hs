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
       vertShader :: String   -- path to vertex shader program
     , fragShader :: String   -- path to fragment shader program
     , texts :: [String] -- paths to texture bindings
     } deriving Show
