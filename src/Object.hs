{-# LANGUAGE OverloadedStrings #-}

module Object
  ( Object (..)
  , defaultObj
  ) where

-- import Control.Monad             (mzero)
-- import Data.Aeson           as A
-- import Data.ByteString.Lazy as B
-- import Data.Maybe                (fromMaybe)

import Linear.V4
import Linear.Matrix (M44, M33, identity)
import Linear (V3(..))

import Controllable
-- import Geometry
import Keyboard
import Material
import Descriptor
  
--------------------------------------------------------------------------------
-- < Object > ------------------------------------------------------------------

data Object
  =  Object
     { scalar      :: Double
     , descriptors :: [Descriptor]
     , material    :: [Material]
     --, transform  :: GLmatrix GLfloat
     , velocity    :: V4 Double
     , driver      :: Controllable
     } deriving Show

defaultObj 
  = Object.Object
    0.0
    []                        --"models/square.pgeo" --(Geo [[]] [] [] [] [] [])
    [defaultMat]
    (V4 0 0 0 0)
    ( Controllable
      (0,0)
      (identity :: M44 Double)
      (V3 0 0 0)
      (Devices (Keyboard keys0 kvs0) (Mouse Nothing Nothing (0,0) [])) )
          where
            keys0  = ( Keys False False False False False False False False False False False False )
            kvs0   = [ fVel, bVel, lVel, rVel, uVel, dVel, pPitch, nPitch, pYaw, nYaw, pRoll, nRoll ]
            fVel   = V3 ( 0  )( 0  )( 999)   -- forwards  velocity
            bVel   = V3 ( 0  )( 0  )(-999)   -- backwards velocity
            lVel   = V3 ( 999)( 0  )( 0  )   -- left      velocity
            rVel   = V3 (-999)( 0  )( 0  )   -- right     velocity
            uVel   = V3 ( 0  )(-999)( 0  )   -- right     velocity
            dVel   = V3 ( 0  )( 999)( 0  )   -- right     velocity
            pPitch = V3 ( 999)( 0  )( 0  )   -- positive  pitch
            nPitch = V3 (-999)( 0  )( 0  )   -- negative  pitch
            pYaw   = V3 ( 0  )( 999)( 0  )   -- positive  yaw
            nYaw   = V3 ( 0  )(-999)( 0  )   -- negative  yaw
            pRoll  = V3 ( 0  )(  0 )( 999)   -- positive  roll
            nRoll  = V3 ( 0  )(  0 )(-999)   -- negative  roll

-- initObjects :: Project -> [Object]
-- initObjects project =
--   fmap path (models project)

-- given file path, return an Object:
-- initObject :: String -> Object
-- initObject model = undefined

-- --------------------------------------------------------------------------------
-- -- < Parser > ------------------------------------------------------------------

-- parse :: FilePath -> IO Object.Object
-- parse filePath =
--   do
--     d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Object.Object)
--     --let geometry' = (geometry . fromEitherDecode) d
--     return $ defaultObj { geoPath = "" }
--       where
--         fromEitherDecode = fromMaybe (defaultObj) . fromEither
--         fromEither d =
--           case d of
--             Left err -> Nothing
--             Right pt -> Just pt

-- -- newtype GeoPath = GeoPath String deriving Show

-- instance FromJSON Object.Object where
--   parseJSON (A.Object o) =
--     Object.Object
--       <$> ((o .: "project") >>= (.: "object") >>= (.: "path"))
--       <*> ((o .: "project") >>= (.: "object") >>= (.: "path"))
--       <*> ((o .: "project") >>= (.: "object") >>= (.: "path"))
--       <*> ((o .: "project") >>= (.: "object") >>= (.: "path"))
--       <*> ((o .: "project") >>= (.: "object") >>= (.: "path"))
--   parseJSON _ = mzero

-- instance FromJSON GeoPath where
--   parseJSON (A.Object o) =
--     do
--       geoPath <- o .: "path"
--       GeoPath <$> parseJSON geoPath
--   parseJSON _ = mzero

-- instance FromJSON Material where
--   parseJSON (A.Object o) =
--     Material
--       <$> o .: "vert"
--       <*> o .: "frag"
--       <*> o .: "textures"
--   parseJSON _ = mzero
