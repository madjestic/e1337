module VGeo where

data VGeo
  =  VGeo
     {
       is :: [[Int]]    -- indices
     , st :: [Int]      -- stride
     , vs :: [[Float]]  -- all attrs as a flat list
     , ms :: [FilePath] -- materials
     , xf :: [[Float]]  -- preTransform
     }
  deriving Show
