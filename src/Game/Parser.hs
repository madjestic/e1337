{-# LANGUAGE OverloadedStrings #-}

module Game.Parser
  ( parse
  ) where

import Control.Monad             (mzero)
import Data.Aeson           as A
import Data.ByteString.Lazy as B
import Data.Maybe                (fromMaybe)
--import Foreign.C                 (CInt)

import Game

-- parse :: FilePath -> IO [Game.Options]
-- parse filePath = 
--   do
--     d <- decodeFileStrict filePath :: IO (Maybe [(Game.Options)])
--     --print d
--     return $
--       case d of
--         Just d -> d
--         Nothing -> []

parse :: FilePath -> IO Game.Options
parse filePath = 
  do
    d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Game.Options)
    --print d
    let name' = (name . fromEitherDecode) d -- TODO as in Geometry readPGeo
        resx' = (resx . fromEitherDecode) d
        resy' = (resy . fromEitherDecode) d
    return $ Game.Options name' resx' resy'
      where
        fromEitherDecode = fromMaybe (Game.Options [] 256 256) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right pt -> Just pt
        
-- < Game Parser > --
-- newtype Name  = Name  String deriving Show
-- newtype ResX  = ResX  Int    deriving Show
-- newtype ResY  = ResY  Int    deriving Show
-- newtype Obj   = Obj   String deriving Show
-- newtype MAT   = MAT   [String] deriving Show

instance FromJSON Game.Options where
  parseJSON (Object o) =
    Game.Options
      <$> ((o .: "project") >>= (.: "name")) 
      <*> ((o .: "project") >>= (.: "resx"))
      <*> ((o .: "project") >>= (.: "resy"))
  parseJSON _ = mzero

-- instance FromJSON Name where
--   parseJSON (Object o) =
--     do
--       name <- o .: "name"
--       Name <$> parseJSON name
--   parseJSON _ = mzero

-- instance FromJSON ResX where
--   parseJSON (Object o) =
--     do
--       resx <- o .: "resx"
--       ResX <$> parseJSON resx
--   parseJSON _ = mzero

-- instance FromJSON ResY where
--   parseJSON (Object o) =
--     do
--       resy <- o .: "resy"
--       ResY <$> parseJSON resy
--   parseJSON _ = mzero
