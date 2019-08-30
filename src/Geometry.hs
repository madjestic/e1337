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
  , getJSON
  , readPGeo
  , readVBO
  ) where

import Control.Monad (mzero)
import Data.Aeson                             hiding (withArray)
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Graphics.Rendering.OpenGL as GL        (Vertex4(..))

import FromVector


instance FromVector Vec3 where
  toVertex4 :: Vec3 -> Vertex4 Double
  toVertex4 (k, l, m) = Vertex4 k l m 1.0  

data Geo
  =
    Geo
     {
       positions :: Positions
     , uv        :: UVs
     , indices   :: [Int]
--     , normal   :: [Vec3]
     }
  | GLGeo
     {
       vs  :: [Float]
     , is :: [Int]
     } 
  deriving Show



-- | TODO : replace Vec3 -> Vec4
type Vec3     = (Double, Double, Double)

-- type Colors    = [Vec3]
type Positions = [Vec3]
type UVs       = [Vec3] 

--type Vec4     =  (Double, Double, Double)
                     
-- < Reading Geo > --------------------------------------------------------
newtype Position = Position [Vec3] deriving Show
newtype UV       = UV       [Vec3] deriving Show
newtype Index    = Index    [Int]  deriving Show

instance FromJSON Geo where
  parseJSON (Object o) =
     Geo
       <$> ((o .: "PGeo") >>= (.: "position"))
       <*> ((o .: "PGeo") >>= (.: "uv"))
       <*> ((o .: "PGeo") >>= (.: "indices"))
  parseJSON _ = mzero

instance FromJSON Position where
    parseJSON (Object o) =
      do
        pts <- o .: "pos"
        Position <$> parseJSON pts
    parseJSON _ = mzero

instance FromJSON UV where
    parseJSON (Object o) =
      do
        uv  <- o .: "uv"
        UV <$> parseJSON uv
    parseJSON _ = mzero

instance FromJSON Index where
    parseJSON (Object o) =
      do
        idx <- o .: "indices"
        Index <$> parseJSON idx
    parseJSON _ = mzero    

getJSON :: FilePath -> IO B.ByteString
getJSON  = B.readFile

readVBO :: FilePath -> IO Geo
readVBO file = 
  do
    d <- decodeFileStrict file :: IO (Maybe ([Float],[Int]))
    return $ case d of
               Just d -> GLGeo (fst d) (snd d)
               Nothing  -> GLGeo [] []

readPGeo :: FilePath -> IO Geo
readPGeo jsonFile =
  do
    d <- (eitherDecode <$> getJSON jsonFile) :: IO (Either String Geo)
    let ps        = (positions . fromEitherDecode) d
    let uvs       = (uv        . fromEitherDecode) d
    let ids       = (indices   . fromEitherDecode) d
    
    return $ Geo ps uvs ids

      where
        fromEitherDecode = fromMaybe (Geo [] [] []) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right ps -> Just ps
    
