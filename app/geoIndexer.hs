{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Text.Lazy.IO as I    hiding (putStrLn)
import Data.ByteString   as BS   (writeFile)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Store                (encode)
       
import Unsafe.Coerce
import System.Environment        (getArgs)

import PGeo
import VGeo

--import Debug.Trace as DT

writeVGeo :: FilePath -> VGeo -> IO ()
writeVGeo fileOut vgeo =
  do
    I.writeFile "../models/debug.vgeo"
      (encodeToLazyText ( is vgeo
                        , st vgeo
                        , vs vgeo
                        , ms vgeo
                        , xf vgeo ))

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
  -- _ <- DT.trace ("geoIndexer.vgeo :" ++ show vgeo) $ return ()
  putStrLn "OK"
  writeBGeo fileOut vgeo
  -- writeVGeo fileOut vgeo
