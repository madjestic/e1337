--  .d8888b. 8888888888 .d88888b. 888b     d8888888888888888888888888888888b.Y88b   d88P 
-- d88P  Y88b888       d88P" "Y88b8888b   d8888888           888    888   Y88bY88b d88P  
-- 888    888888       888     88888888b.d88888888           888    888    888 Y88o88P   
-- 888       8888888   888     888888Y88888P8888888888       888    888   d88P  Y888P    
-- 888  88888888       888     888888 Y888P 888888           888    8888888P"    888     
-- 888    888888       888     888888  Y8P  888888           888    888 T88b     888     
-- Y88b  d88P888       Y88b. .d88P888   "   888888           888    888  T88b    888     
--  "Y8888P888888888888 "Y88888P" 888       8888888888888    888    888   T88b   888     

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Geometry
  ( Geo(..)
  , FromGeo(..)
  , Vec3(..)
  , getJSON
  , readPGeo
  , readVBOGeo
  ) where

import Control.Monad (mzero)
import Data.Aeson                             
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Graphics.Rendering.OpenGL as GL        (Vertex4(..))

import FromVector

import Debug.Trace   as DT

instance FromVector Vec3 where
  toVertex4 :: Vec3 -> Vertex4 Double
  toVertex4 (k, l, m) = Vertex4 k l m 1.0  

data Geo
  =  Geo
     {
       indices   :: [[Int]]  
     , alpha     :: [Float] -- 1
     , color     :: [Vec3]  -- 3
     , normal    :: [Vec3]  -- 3
     , uv        :: [Vec3]  -- 3
     , position  :: [Vec3]  -- 3 -> 13+1 -> 14 stride
     , materials :: [String]
     }
  | GLGeo
     {
       vs  :: [Float] -- all attrs as a flat list
     , is  :: [[Int]]   -- indices
     } 
  deriving Show

class FromGeo a where
  fromGeo :: Geo -> a

-- | TODO : replace Vec3 -> Vec4
type Vec3     = (Double, Double, Double)
--type Vec4     =  (Double, Double, Double)
                     
instance FromJSON Geo where
  parseJSON (Object o) =
     Geo
       <$> ((o .: "PGeo") >>= (.: "indices"))
       <*> ((o .: "PGeo") >>= (.: "Alpha"))
       <*> ((o .: "PGeo") >>= (.: "Cd"))
       <*> ((o .: "PGeo") >>= (.: "N"))
       <*> ((o .: "PGeo") >>= (.: "uv"))
       <*> ((o .: "PGeo") >>= (.: "position"))
       <*> ((o .: "PGeo") >>= (.: "material"))
  parseJSON _ = mzero

getJSON :: FilePath -> IO B.ByteString
getJSON  = B.readFile

-- TODO : read P/VBOGeo should return grouped list of materials
readVBOGeo :: FilePath -> IO Geo
readVBOGeo file = 
  do
    d <- decodeFileStrict file :: IO (Maybe ([Float],[[Int]]))
    return $ case d of
               Just d -> GLGeo (fst d) (snd d)
               Nothing  -> GLGeo [] [[]]

readPGeo :: FilePath -> IO Geo
readPGeo jsonFile =
  do
    --_ <- DT.trace ("trace 1_0_1: " ++ show jsonFile) $ return ()
    d <- (eitherDecode <$> getJSON jsonFile) :: IO (Either String Geo)
    -- print d
    let ids = (indices   . fromEitherDecode) d
        as  = (alpha     . fromEitherDecode) d
        cds = (color     . fromEitherDecode) d
        ns  = (normal    . fromEitherDecode) d
        uvs = (uv        . fromEitherDecode) d
        ps  = (position  . fromEitherDecode) d
        ms  = (materials . fromEitherDecode) d
    -- _ <- DT.trace ("trace 1_0_2: ids:" ++ show ids) $ return ()
    -- _ <- DT.trace ("trace 1_0_2: ps:" ++ show ps) $ return ()
    return $ Geo ids as cds ns uvs ps ms

      where
        fromEitherDecode = fromMaybe (Geo [[]] [] [] [] [] [] []) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt
            
    
