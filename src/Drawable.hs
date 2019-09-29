{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Drawable
  ( ToDrawable(..)
  , Drawable(..)
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
import FromVector

import Debug.Trace as DT

data Drawable
  =  Drawable
    { verts  :: [GLfloat]
    , ids    :: [GLuint]
    } deriving Show

class ToDrawable a where
  toDrawable :: a -> IO Drawable

instance FromGeo (IO Drawable) where  
  fromGeo :: Geo -> IO Drawable
  fromGeo geo = do
    let stride = 14 -- TODO : stride <- attr sizes
    --(vs, idx) <- (indexedVAO ids' as' cds' ns' uv' ps' stride) :: IO ([GLfloat],[GLuint])
    vs <- (toVAO ids' as' cds' ns' uv' ps') :: IO [GLfloat]
    -- _ <- DT.trace ("vs: "   ++ show vs) $ return ()
    -- _ <- DT.trace ("ids': " ++ show ids') $ return ()
    --return (Drawable vs ids')
    return (Drawable vs uid)
      where
        ids' = map fromIntegral $ indices geo
        uid  = (map fromIntegral [0..((length ids')-1)] :: [GLuint])
        as'  = alpha geo
        cds' = map (\ (r, g, b) -> Vertex3   r g b) $ color  geo
        ns'  = map (\ (x, y, z) -> Vertex3   x y z) $ normal geo
        uv'  = map (\ (k, l, m) -> TexCoord3 k l m) $ uv     geo
        ps'  = map toVertex4 $ positions geo

indexedVAO :: [GLuint]
           -> [Float]
           -> [Vertex3 Double]
           -> [Vertex3 Double]
           -> [TexCoord3 Double]
           -> [Vertex4 Double]
           -> Int
           -> IO ([GLfloat],[GLuint])
indexedVAO idx as cds ns ts ps st =
  do
    vao <- toVAO idx as cds ns ts ps
    let iListSet = (fmap (\x -> x) $ indexed $ DS.toList $ DS.fromList $ chunksOf st $ vao) :: [(Int,[GLfloat])] --indexedListSet vao st
        iList    = (indexed $ chunksOf st vao)
        idx = fmap (\(i,_) -> (fromIntegral i)) (matchLists iListSet iList)
        vx  = concat $ fmap (\x -> snd x) iListSet
    return (vx, idx)

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

indexedListSet :: [GLfloat] -> Int -> [(Int,[GLfloat])]
indexedListSet vao st =
  fmap (\x -> x) $ indexed $ DS.toList $ DS.fromList $ chunksOf st $ vao

toVAO :: [GLuint]
      -> [Float]
      -> [Vertex3 Double]
      -> [Vertex3 Double]
      -> [TexCoord3 Double]
      -> [Vertex4 Double]
      -> IO [GLfloat]
toVAO idx as cds ns ts ps  =
  return $ concat $
    fmap (\i ->
            (\x -> [x])                                            (as !!(fromIntegral       i)) ++ -- 1
            (\(Vertex3   r g b)   -> fmap realToFrac [r,g,b])      (cds!!(fromIntegral       i)) ++ -- 3
            (\(Vertex3   x y z)   -> fmap realToFrac [x,y,z])      (ns !!(fromIntegral       i)) ++ -- 3
            (\(TexCoord3 u v w)   -> fmap realToFrac [u, v, w])    (ts !!(fromIntegral       i)) ++ -- 3
            (\(Vertex4   x y z w) -> fmap realToFrac [x, y, z, w]) (ps !!(fromIntegral (idx!!i)))   -- 4 -> stride 14
         ) iter
      where
        iter = [0..(length idx)-1]
