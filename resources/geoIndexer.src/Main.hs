{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Text.Lazy            (Text)
import Data.Text.Lazy.IO as I    hiding (putStrLn)
import Data.ByteString   as BS   (writeFile)
import Blaze.ByteString.Builder  (toByteString)
import Data.Binary.Builder       (toLazyByteString, Builder)
import Data.Text.Encoding        (encodeUtf8)
import Data.Aeson.Text           (encodeToLazyText, encodeToTextBuilder)
import Data.Aeson                (fromEncoding, toEncoding, ToJSON)
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
import Data.Store        as DST
       
import Unsafe.Coerce
import System.Environment        (getArgs)

import PGeo
import VGeo
import Utils

import Debug.Trace as DT

-- writeVGeo :: FilePath -> VGeo -> IO ()
-- writeVGeo fileOut vgeo =
--   do
--     I.writeFile fileOut (encodeToLazyText ( is vgeo
--                                           , st vgeo
--                                           , vs vgeo
--                                           , ms vgeo
--                                           , xf vgeo ))

writeBGeo :: FilePath -> VGeo -> IO ()
writeBGeo fileOut vgeo =
  do
    BS.writeFile fileOut $ encode $ ( is vgeo, st vgeo, vs vgeo, ms vgeo, xf vgeo )

main :: IO ()
main = do
  args <- getArgs
  let fileIn  =  (unsafeCoerce (args!!0) :: FilePath)
      fileOut =  (unsafeCoerce (args!!1) :: FilePath)
  
  pgeo <- readPGeo fileIn
  putStrLn "running indexer..."
  let vgeo = fromPGeo pgeo -- goes wrong
  _ <- DT.trace ("geoIndexer.vgeo :" ++ show vgeo) $ return ()
  putStrLn "OK"
  --writeVGeo fileOut vgeo
  writeBGeo fileOut vgeo
  -- print $ "is :" ++ (show $ is vgeo)
  -- I.writeFile fileOut (encodeToLazyText ( is vgeo
  --                                       , st vgeo
  --                                       , vs vgeo
  --                                       , ms vgeo
  --                                       , xf vgeo ))
