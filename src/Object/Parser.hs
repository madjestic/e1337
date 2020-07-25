{-# LANGUAGE OverloadedStrings #-}

module Object.Parser
  ( parse
  ) where

import Linear.V4
import Control.Monad             (mzero)
import Data.Aeson           as A
import Data.ByteString.Lazy as B
import Data.Maybe                (fromMaybe)

import Object

parse :: FilePath -> IO Object.Object
parse filePath =
  do
    d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Object.Object)
    let geometry' = (geometry . fromEitherDecode) d
    return $ defaultObj { geometry = geometry' }
      where
        fromEitherDecode = fromMaybe (defaultObj) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt

newtype GeoPath = GeoPath String deriving Show

instance FromJSON Object.Object where
  parseJSON (A.Object o) =
    Object.Object
      <$> ((o .: "project") >>= (.: "object") >>= (.: "path"))
      <*> ((o .: "project") >>= (.: "object") >>= (.: "path"))
      <*> ((o .: "project") >>= (.: "object") >>= (.: "path"))
      <*> ((o .: "project") >>= (.: "object") >>= (.: "path"))
      <*> ((o .: "project") >>= (.: "object") >>= (.: "path"))
  parseJSON _ = mzero

instance FromJSON GeoPath where
  parseJSON (A.Object o) =
    do
      geoPath <- o .: "path"
      GeoPath <$> parseJSON geoPath
  parseJSON _ = mzero
