{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Material
  ( Material (..)
  , name
  , defaultMat
  , readMaterial
  ) where  

import Control.Monad (mzero)
import Data.Aeson                             
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Control.Lens

data Material
  =  Material
     {
       _name       :: String
     , _vertShader :: FilePath   -- path to vertex shader program
     , _fragShader :: FilePath   -- path to fragment shader program
     , _textures   :: [FilePath] -- paths to texture bindings
     } deriving Show

name :: Lens' Material String
name = lens _name (\material newName -> Material { _name = newName })

vertShader :: Lens' Material FilePath
vertShader = lens _vertShader (\material newVertShader -> Material { _vertShader = newVertShader })

fragShader :: Lens' Material FilePath
fragShader = lens _fragShader (\material newFragShader -> Material { _fragShader = newFragShader })

textures :: Lens' Material [FilePath]
textures = lens _textures (\material newTextures -> Material { _textures = newTextures })

defaultMat
  = Material
    "foobar"
    -- "mat/mandelbrot/shader.vert"
    -- "mat/mandelbrot/shader.frag"
    "mat/const/const.vert"
    "mat/const/const.frag"
    -- "shaders/shader.vert"
    -- "shaders/shader.frag"
    []


instance FromJSON Material where
  parseJSON (Object o) =
     Material
       <$> ((o .: "Material") >>= (.: "name"))
       <*> ((o .: "Material") >>= (.: "Shaders") >>= (.: "vertex"))
       <*> ((o .: "Material") >>= (.: "Shaders") >>= (.: "fragment"))
       <*> ((o .: "Material") >>= (.: "textures"))
  parseJSON _ = mzero

-- readMaterial "mat/square/constant_A"
-- Material {name = "constant_A", vertShader = "/mat/square/Constant_A/shader.vert", fragShader = "/mat/square/Constant_A/shader.frag", textures = [""]}
readMaterial :: FilePath -> IO Material
readMaterial jsonFile =
  do
    d <- (eitherDecode <$> B.readFile jsonFile) :: IO (Either String Material)
    let name'       = (_name       . fromEitherDecode) d
        vertShader' = (_vertShader . fromEitherDecode) d
        fragShader' = (_fragShader . fromEitherDecode) d
        textures'   = (_textures   . fromEitherDecode) d
    return $ Material name' vertShader' fragShader' textures'
      where
        fromEitherDecode = fromMaybe (Material "" "" "" []) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt
            
