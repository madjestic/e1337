{-# LANGUAGE OverloadedStrings #-}

module Project.Parser
  ( parse
--  , parseMod
  ) where

import Control.Monad             (mzero)
import Data.Aeson           as A
import Data.ByteString.Lazy as B
import Data.Maybe                (fromMaybe)

import Project
import Texture

--import Debug.Trace as DT

parse :: FilePath -> IO Project
parse filePath =
  do
    d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Project)
    let name'     = (name      . fromEitherDecode) d
        resx'     = (resx      . fromEitherDecode) d
        resy'     = (resy      . fromEitherDecode) d
        models'   = (_models   . fromEitherDecode) d
        textures' = (_textures . fromEitherDecode) d
        camera'   = (_cameraP  . fromEitherDecode) d
    return $ Project name' resx' resy' models' textures' camera'
      where
        fromEitherDecode = fromMaybe (Project "foobar" (-1) (-1) [] [] []) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt

instance FromJSON Project where
  parseJSON (Object o) =
    Project
      <$> ((o .: "project") >>= (.: "name")) 
      <*> ((o .: "project") >>= (.: "resx"))
      <*> ((o .: "project") >>= (.: "resy"))
      <*> ((o .: "project") >>= (.: "models"))
      <*> ((o .: "project") >>= (.: "textures"))
      <*> ((o .: "project") >>= (.: "camera"))
  parseJSON _ = mzero

instance FromJSON Model where
  parseJSON (Object o) =
    Model
      <$> o .: "path"
  parseJSON _ = mzero

instance FromJSON Texture where
  parseJSON (Object o) =
    Texture
      <$> o .: "path"
  parseJSON _ = mzero
