-- OBSOLETE
-----------

--  .d8888b. 8888888888 .d88888b.  
-- d88P  Y88b888       d88P" "Y88b 
-- 888    888888       888     888 
-- 888       8888888   888     888 
-- 888  88888888       888     888 
-- 888    888888       888     888 
-- Y88b  d88P888       Y88b. .d88P 
--  "Y8888P888888888888 "Y88888P"

{-# LANGUAGE OverloadedStrings #-}

module Geo
  ( readPGeo
  , jsonFile
  , Geo
  , position
  , uv
  ) where

import Control.Monad (mzero)
import Data.Aeson                             hiding (withArray)
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Graphics.Rendering.OpenGL              (Vertex4)
import Data.Text                              (Text)


data Geo
  =
    Geo
    {
      position :: [Vec3]
    , uv       :: [Vec3]
    }
  deriving Show

type Vec3     = (Double, Double, Double)
                     
-- < Reading Geo > --------------------------------------------------------
newtype Position = Position [Vec3] deriving Show
newtype UV       = UV       [Vec3] deriving Show

instance FromJSON Geo where
  parseJSON (Object o) =
     Geo
       <$> ((o .: "PGeo") >>= (.: "position"))
       <*> ((o .: "PGeo") >>= (.: "uv"))
  parseJSON _ = mzero

instance FromJSON Position where
    parseJSON (Object o) =
      do
        pts <- o .: "position"
        Position <$> parseJSON pts
    parseJSON _ = mzero

instance FromJSON UV where
    parseJSON (Object o) =
      do
        uv <- o .: "uv"
        UV <$> parseJSON uv
    parseJSON _ = mzero

type Positions = [Vertex4 Double] 
data Transform = Transform {}

jsonFile :: FilePath
jsonFile = "src/model.pgeo"

getJSON :: FilePath -> IO B.ByteString
getJSON  = B.readFile

readPGeo :: FilePath -> IO Geo
readPGeo jsonFile =
  do
    d <- (eitherDecode <$> getJSON jsonFile) :: IO (Either String Geo)
    let ps        = (position . fromEitherDecode) d
    let uvs       = (uv       . fromEitherDecode) d
    return $ Geo ps uvs

      where
        fromEitherDecode = fromMaybe (Geo [] []) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right ps -> Just ps
    
