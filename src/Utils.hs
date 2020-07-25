{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Utils
  ( toVAO
  , toIdxVAO
  , Utils.fromList
  , (<$.>)
  , (<*.>)
  ) where

import Graphics.Rendering.OpenGL as GL ( Vertex4(..)
                                       , Vertex3(..)
                                       , TexCoord3(..)
                                       , GLuint
                                       , GLfloat )

import Data.Set                  as DS (fromList, toList)
import Data.List.Index                 (indexed)
import Data.List                       (elemIndex, sortBy, sort)
import Control.Lens.Combinators        (iset, indexing, each)
import Linear.V3
import Linear.V4
import Linear.Matrix -- (M44, M33, identity, translation, fromQuaternion, (!*!), mkTransformationMat)
import Data.VectorSpace

import Debug.Trace as DT

instance VectorSpace (V3 Double) Double where
  zeroVector                   = (V3 0 0 0)
  (*^) s (V3 x y z)            = (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3 x y z) (V3 k l m) = (V3 (x+k) (y+l) (z+m))
  dot    (V3 x y z) (V3 k l m) = (x*k) + (y*l) + (z*m)

toVAO
  :: [[Int]]
  -> [Float]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [[[Float]]]

toVAO idxs as cds ns ts ps = vaos
  where vaos =
          fmap
          (\idx ->
              (fmap (\i ->
                        (\x         -> [x])                       (as !!(idx!!i)) ++ -- 1
                        (\(r,g,b)   -> fmap realToFrac [r,g,b])   (cds!!(idx!!i)) ++ -- 3
                        (\(x,y,z)   -> fmap realToFrac [x,y,z])   (ns !!(idx!!i)) ++ -- 3
                        (\(u,v,w)   -> fmap realToFrac [u,v,w])   (ts !!(idx!!i)) ++ -- 3
                        (\(x,y,z)   -> fmap realToFrac [x,y,z])   (ps !!(idx!!i))    -- 3 -> 13 stride
                    ) [0..(length idx)-1])
          ) idxs

-- | [Float]  ~= vertex
--  [[Float]] ~= VAO
toIdxVAO :: Int -> [[Float]] -> ([Int],[Float])
toIdxVAO stride vao = (idx, idxVAO)
  where
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
    
fromList :: [Float] -> M44 Double
fromList xs' = V4 x y z w
  where
    x  = V4 (xs!!0 ) (xs!!1 ) (xs!!2 ) (xs!!3 ) 
    y  = V4 (xs!!4 ) (xs!!5 ) (xs!!6 ) (xs!!7 ) 
    z  = V4 (xs!!8 ) (xs!!9 ) (xs!!10) (xs!!11) 
    w  = V4 (xs!!12) (xs!!13) (xs!!14) (xs!!15)
    xs = fmap realToFrac xs' :: [Double]

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = fmap

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

