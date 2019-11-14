{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Utils
  ( toVAO
  , toIdxVAO
  ) where

import Graphics.Rendering.OpenGL as GL ( Vertex4(..)
                                       , Vertex3(..)
                                       , TexCoord3(..)
                                       , GLuint
                                       , GLfloat )

import Data.Set                  as DS (fromList, toList)
import Data.List.Split                 (chunksOf)
import Data.List.Index                 (indexed)
import Data.List                       (elemIndex, sortBy, sort)

import Geometry
       
import Debug.Trace as DT

toVAO
  :: [[Int]]
  -> [Float]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [[[Float]]]

toVAO idxs as cds ns ts ps = (DT.trace ("vaos :" ++ show vaos) $ vaos)
  where vaos =
          fmap
          (\idx ->
              (fmap (\i ->
                        (\x         -> [x])                       (as !!i) ++ -- 1
                        (\(r,g,b)   -> fmap realToFrac [r,g,b])   (cds!!i) ++ -- 3
                        (\(x,y,z)   -> fmap realToFrac [x,y,z])   (ns !!i) ++ -- 3
                        (\(u,v,w)   -> fmap realToFrac [u,v,w])   (ts !!i) ++ -- 3
                        (\(x,y,z)   -> fmap realToFrac [x,y,z])   (ps !!(idx!!i)) -- 3 -> stride 13 -- this shit draws different tris
                    ) [0..(length idx)-1])
          ) idxs

-- | [Float]  ~= vertex
--  [[Float]] ~= VAO
toIdxVAO :: Int -> [[Float]] -> ([Int],[Float])
toIdxVAO stride vao = (idx, idxVAO)
  where
    --idxVAO = undefined
    --iListSet = indexed $ DS.toList $ DS.fromList $ chunksOf stride $ vao     :: [(Int,[GLfloat])]
    iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
    iList    = indexed vao                                                   :: [(Int, [GLfloat])]
    idx      = fmap (\(i,_) -> (fromIntegral i)) (matchLists iListSet iList) :: [Int]
    idxVAO   = concat $ fmap (\x -> snd x) iListSet                          :: [Float]

-- | matchLists - cross-match 2 listst, replacing the elements of list2 with matching
-- |          with elements of list1, concatenating the non-matched elements.
-- |   il - indexed list
-- |  nil - non-indexed list
matchLists :: [(Int, [GLfloat])] -> [(Int, [GLfloat])] -> [(Int, [GLfloat])]
matchLists il nil =
  fmap (mFunc il ) nil -- | mFunc - matching function
  where
    -- | il      - indexed list
    -- | nile    - non indexed list element
    -- | Replaces the target element with the first match from the matching list il
    mFunc il nile@(iy, cy) =
      (\x -> case x of
               Just idx -> il!!idx
               Nothing  -> (-iy, cy) ) nili -- | if a unique index idx found - flip the sign
                                            -- | the idea idx to separate normal indexes
                                            -- | and unique indexes -> [idx:uidx] later
      where
        nili = elemIndex cy cxs
        cxs  = fmap (\(i,s) -> s) il :: [[GLfloat]]
    
