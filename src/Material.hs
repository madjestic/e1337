{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Material
  ( Material (..)
  , defaultMat
  , readMaterial
  ) where  

import Control.Monad (mzero)
import Data.Aeson                             
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as B

data Material
  =  Material
     {
       name       :: String
     , vertShader :: FilePath   -- path to vertex shader program
     , fragShader :: FilePath   -- path to fragment shader program
     , textures   :: [FilePath] -- paths to texture bindings
     } deriving Show


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
    let name'       = (name       . fromEitherDecode) d
        vertShader' = (vertShader . fromEitherDecode) d
        fragShader' = (fragShader . fromEitherDecode) d
        textures'   = (textures   . fromEitherDecode) d
    return $ Material name' vertShader' fragShader' textures'
      where
        fromEitherDecode = fromMaybe (Material "" "" "" []) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt
            
