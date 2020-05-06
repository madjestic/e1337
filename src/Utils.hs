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
import Control.Lens.Combinators        (iset, indexing, each)

--import Geometry
       
import Debug.Trace as DT

toVAO
  :: [[Int]]
  -> [Float]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [[[Float]]]

-- toVAO idxs as cds ns ts ps = (
--   DT.trace ("as :" ++ show as) $
--   DT.trace ("cds :" ++ show cds) $
--   DT.trace ("ns :" ++ show ns) $
--   DT.trace ("ts :" ++ show ts) $
--   DT.trace ("ps :" ++ show ps) $
--   DT.trace ("vaos :" ++ show vaos) $ vaos)
-- toVAO idxs as cds ns ts ps = vaos
--   where vaos =
--           fmap
--           (\idx ->
--               (fmap (\i ->
--                         (\x         -> [x])                       (as !!i) ++ -- 1
--                         (\(r,g,b)   -> fmap realToFrac [r,g,b])   (cds!!(idx!!i)) ++ -- 3
--                         (\(x,y,z)   -> fmap realToFrac [x,y,z])   (ns !!i) ++ -- 3
--                         (\(u,v,w)   -> fmap realToFrac [u,v,w])   (ts !!i) ++ -- 3
--                         (\(x,y,z)   -> fmap realToFrac [x,y,z])   (ps !!(idx!!i)) -- 3 -> stride 13 -- this shit draws different tris
--                     ) [0..(length idx)-1])
--           ) idxs
toVAO idxs as cds ns ts ps = vaos
  where vaos =
          fmap
          (\(idx, idx') ->
              (fmap (\i ->
                        (\x         -> [x])                       (as !!(idx!!i)) ++ -- 1
                        (\(r,g,b)   -> fmap realToFrac [r,g,b])   (DT.trace (" toVAO.idx" ++ show idx ++
                                                                             "\n toVAO.idx'" ++ show idx' ++
                                                                             -- "\n toVAO.(cds!!i)" ++ show ((cds!!i)) ++
                                                                             "\n toVAO.(cds!!(idx!!i))" ++ show (cds!!(idx!!i)) ++
                                                                             "\n toVAO.(cds)" ++ show ((cds)) ++
                                                                             "\n")
                                                                    $ (cds!!(idx!!i))) ++ -- 3
                        (\(x,y,z)   -> fmap realToFrac [x,y,z])   (ns !!(idx!!i)) ++ -- 3
                        (\(u,v,w)   -> fmap realToFrac [u,v,w])   (ts !!(idx!!i)) ++ -- 3
                        (\(x,y,z)   -> fmap realToFrac [x,y,z])   (DT.trace (" ps :" ++ show ps ++
                                                                            "\n (idx'!!i)" ++ show (idx'!!i) ++
                                                                            "\n (idx!!i)" ++ show (idx!!i) ++
                                                                            --"\n idx' :" ++ show idx' ++
                                                                            "\n")
                                                                   $ ps !!(idx!!i)) -- 3 -> stride 13 -- this shit draws different tris
                    ) [0..(length idx)-1])
          ) (zip idxs idxs')
        idxs' = iset (indexing $ each.each) id $ idxs
        -- idxs' = chunksOf (length (idxs!!0)) . fmap fst . zip [0..] . concat $ idxs


-- | [Float]  ~= vertex
--  [[Float]] ~= VAO
-- toIdxVAO :: Int -> [[Float]] -> ([Int],[Float])
-- toIdxVAO stride vao = (idx, idxVAO)
--   where
--     iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
--     iList    = indexed vao                                                   :: [(Int, [GLfloat])]
--     idx      = fmap (\(i,_) -> (fromIntegral i)) (matchLists iListSet iList) :: [Int]
--     idxVAO   = concat $ fmap (\x -> snd x) iListSet                          :: [Float]

toIdxVAO :: Int -> [[Float]] -> ([Int],[Float])
toIdxVAO stride vao = (idx, idxVAO)
  where
    -- iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
    iList    = indexed vao                                                   -- :: [(Int, [GLfloat])]
    idx      = fmap (\(i,_) -> (fromIntegral i)) (DT.trace ("toIdxVAO.iList :" ++ show iList) $ iList) :: [Int]
    idxVAO   = concat $ fmap (\x -> snd x) iList                             -- :: [Float]

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
    
