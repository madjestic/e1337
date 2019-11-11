{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Text.Lazy            (Text)
import Data.Text.Lazy.IO as I    
import Data.Aeson.Text           (encodeToLazyText)
import Data.Aeson                (ToJSON)
import Data.List.Split           (chunksOf)
import Data.List.Index           (indexed)
import Data.List                 (elemIndex)
import Data.Vector       as DV   (fromList, toList, zipWith
                                 , (!)
                                 , Vector (..))
import Data.Set          as DS   (fromList, toList)
import Graphics.Rendering.OpenGL ( GLfloat (..)
                                 , GLuint (..)
                                 , Vertex4 (..)
                                 , TexCoord3 (..))
import Unsafe.Coerce
import System.Environment        (getArgs)

import Geometry
import Utils

import Debug.Trace as DT

    -- | PGeo -> VGeo
    -- | Stride might be necessary to come out of Houdini material info, rather than be fixed for all materials like now.
    -- * TODO : replace hardcoded stride value with Houdini material data driven value.       
toVGeo :: Geo -> Geo
toVGeo (PGeo idx as cs ns uv ps ms) = (VGeo idxs st vaos ms)
  where
    stride = 13
    --_ = (DT.trace ("idx :" ++ show $ fromList idx) ())
    --vao = DT.trace ("toVAO :" ++ show (toVAO idx as cs ns uv ps)) $ (toVAO idx as cs ns uv ps)
    (idxs, vaos) = unzip $ fmap (toIdxVAO stride) (toVAO idx as cs ns uv ps)
    st           = take (length vaos) $ repeat stride

main :: IO ()
main = do
  args <- getArgs
  let fileIn  =  (unsafeCoerce (args!!0) :: FilePath)
      fileOut =  (unsafeCoerce (args!!1) :: FilePath)

  
  pgeo <- readPGeo fileIn
  let vgeo = toVGeo pgeo
  print $ "is :" ++ (show $ is vgeo)
  
  I.writeFile fileOut (encodeToLazyText ( is vgeo
                                        , st vgeo
                                        , vs vgeo
                                        , ms vgeo))
