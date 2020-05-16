module VGeo where

data VGeo
  =  VGeo
     {
       is :: [[Int]]    -- indices
     , st :: [Int]      -- strides
     , vs :: [[Float]]  -- all attrs as a flat list
     , ms :: [FilePath] -- materials
     , xf :: [[Float]]  -- preTransforms
     }
  deriving Show
