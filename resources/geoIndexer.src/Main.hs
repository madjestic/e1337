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

import PGeo
import VGeo
import Utils

-- import Debug.Trace as DT

main :: IO ()
main = do
  args <- getArgs
  let fileIn  =  (unsafeCoerce (args!!0) :: FilePath)
      fileOut =  (unsafeCoerce (args!!1) :: FilePath)

  
  pgeo <- readPGeo fileIn
  -- let vgeo = fromPGeo (DT.trace ("pgeo :" ++ show pgeo) $ pgeo)
  let vgeo = fromPGeo pgeo
  -- print $ "is :" ++ (show $ is vgeo)

  -- I.writeFile fileOut (encodeToLazyText
  --                      ( VGeo
  --                        (is vgeo)
  --                        (st vgeo)
  --                        (vs vgeo)
  --                        (ms vgeo) ))
  
  I.writeFile fileOut (encodeToLazyText ( is vgeo
                                        , st vgeo
                                        , vs vgeo
                                        , ms vgeo
                                        , xf vgeo ))
