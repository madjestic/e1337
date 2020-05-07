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
module PGeo
  ( PGeo(..)
  , Vec3(..)
--  , getJSON
  , readPGeo
  , readVGeo
  , readBGeo
  , fromPGeo
  ) where

import Control.Monad (mzero)
import Data.Aeson                             
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import Graphics.Rendering.OpenGL      as GL   (Vertex4(..))
import Data.Store                     as DS

import FromVector
import VGeo
import Utils

import Debug.Trace   as DT

instance FromVector Vec3 where
  toVertex4 :: Vec3 -> Vertex4 Double
  toVertex4 (k, l, m) = Vertex4 k l m 1.0  

data PGeo
  =  PGeo
     {
       ids   :: [[Int]]  
     , as    :: [Float] -- 1
     , cs    :: [Vec3]  -- 3
     , ns    :: [Vec3]  -- 3
     , uvws  :: [Vec3]  -- 3
     , ps    :: [Vec3]  -- 3 -> 13+1 -> 14 stride
     , mats  :: [String]
     , xform :: [[Float]] -- [xform M44]
     }
  deriving Show

-- class FromPGeo a where
--   fromPGeo :: PGeo -> a
--   fromPGeo' :: PGeo -> [a]

-- | TODO : replace Vec3 -> Vec4
type Vec3 = (Double, Double, Double)
type Vec4 = (Double, Double, Double, Double)
                     
instance FromJSON PGeo where
  parseJSON (Object o) =
     PGeo
       <$> ((o .: "PGeo") >>= (.: "indices"))
       <*> ((o .: "PGeo") >>= (.: "Alpha"))
       <*> ((o .: "PGeo") >>= (.: "Cd"))
       <*> ((o .: "PGeo") >>= (.: "N"))
       <*> ((o .: "PGeo") >>= (.: "uv"))
       <*> ((o .: "PGeo") >>= (.: "position"))
       <*> ((o .: "PGeo") >>= (.: "material"))
       <*> ((o .: "PGeo") >>= (.: "xform"))
  parseJSON _ = mzero

-- getJSON :: FilePath -> IO BL.ByteString
-- getJSON  = BL.readFile

readBGeo :: FilePath -> IO VGeo
readBGeo file = 
  do
    bs <- BS.readFile file
    return $ case (DS.decode bs) of
               Right d@(idxs, st, vaos, mats, xform) -> VGeo idxs st vaos mats xform
               Left _ -> VGeo [[]] [] [[]] [] [[]]

readVGeo :: FilePath -> IO VGeo
readVGeo file = 
  do
    d <- decodeFileStrict file :: IO (Maybe ([[Int]],[Int],[[Float]],[String],[[Float]]))
    return $ case d of
               Just d@(idxs, st, vaos, mats, xform) -> VGeo idxs st vaos mats xform
               Nothing  -> VGeo [[]] [] [[]] [] [[]]

readPGeo :: FilePath -> IO PGeo
readPGeo jsonFile =
  do
    --_ <- DT.trace ("trace 1_0_1: " ++ show jsonFile) $ return ()
    d <- (eitherDecode <$> BL.readFile jsonFile) :: IO (Either String PGeo)
    --print d
    let ids'  = (ids   . fromEitherDecode) d
        as'   = (as    . fromEitherDecode) d
        cs'   = (cs    . fromEitherDecode) d
        ns'   = (ns    . fromEitherDecode) d
        uvws' = (uvws  . fromEitherDecode) d
        ps'   = (ps    . fromEitherDecode) d
        ms'   = (mats  . fromEitherDecode) d
        xf'   = (xform . fromEitherDecode) d
    -- _ <- DT.trace ("trace 1_0_2: ids':" ++ show ids') $ return ()
    -- _ <- DT.trace ("trace 1_0_2: ps':" ++ show ps') $ return ()
    return $ PGeo ids' as' cs' ns' uvws' ps' ms' xf'

      where
        fromEitherDecode = fromMaybe (PGeo [[]] [] [] [] [] [] [] [[]]) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt

-- fromPGeo (PGeo idx' as' cs' ns' uvw' ps' ms' xf') = (VGeo (DT.trace ("fromPGeo.idxs :" ++ show idxs) $ idxs) st (DT.trace ("fromPGeo.vaos :" ++ show vaos) $ vaos) ms' xf')
fromPGeo :: PGeo -> VGeo
fromPGeo (PGeo idx' as' cs' ns' uvw' ps' ms' xf') = (VGeo idxs st vaos ms' xf')
  where
    stride = 13 -- TODO: make it more elegant, right now VBO's are hard-coded to be have stride = 13...
    vao = (toVAO idx' as' cs' ns' uvw' ps')
    (idxs, vaos) = unzip $ fmap (toIdxVAO stride) vao
    -- (idxs, vaos) = unzip $ fmap (toIdxVAO stride) (toVAO
    --                                                (DT.trace ("fromPGeo.idx' :" ++ show idx') $ idx')
    --                                                (DT.trace ("fromPGeo.as' :" ++ show as') $ as')
    --                                                (DT.trace ("fromPGeo.cs' :" ++ show cs') $ cs')
    --                                                (DT.trace ("fromPGeo.ns' :" ++ show ns') $ ns')
    --                                                (DT.trace ("fromPGeo.uvw' :" ++ show uvw') $ uvw')
    --                                                (DT.trace ("fromPGeo.ps' :" ++ show ps') $ ps'))
    -- (idxs, vaos) = unzip $ fmap (toIdxVAO stride) (toVAO idx' as' cs' ns' uvw' ps')
    st           = take (length vaos) $ repeat stride
