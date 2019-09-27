{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Text.Lazy            (Text)
import Data.Text.Lazy.IO as I    
import Data.Aeson.Text           (encodeToLazyText)
import Data.Aeson                (ToJSON)
import Data.List.Split           (chunksOf)
import Data.List.Index           (indexed)
import Data.List                 (elemIndex)
import Data.Vector       as DV   (fromList, toList, zipWith
                                 , (!)
                                 , Vector (..))
import Data.Set          as DS   (fromList, toList)
import Graphics.Rendering.OpenGL ( GLfloat (..)
                                 , GLuint (..)
                                 , Vertex4 (..)
                                 , TexCoord3 (..))
import Unsafe.Coerce
import System.Environment        (getArgs)

import Geometry
import Drawables
import Shape2D

toTexCoord3 :: (a, a, a) -> TexCoord3 a
toTexCoord3 (k, l, m) = TexCoord3 k l m

toGLuint :: Int -> GLuint
toGLuint x = fromIntegral x

instance Drawables Geo where
  toDrawable =
    (\x -> case x of
             Geo ps uv ids -> toGLGeo (Geo ps uv ids)
             GLGeo vs is   -> return $ Drawable vs is'
               where
                 is'  = map toGLuint is)

toGLGeo :: Geo -> IO Drawable
toGLGeo geo = do
  let stride = 7 -- TODO : stride <- attr sizes
  (vs, idx) <- indexedVAO ps' uv' ids' stride
  return (Drawable vs idx) -- $ Drawable ps' uv' ids'
    where
      ps'  = map toVertex4   $ positions geo
      uv'  = map toTexCoord3 $ uv        geo
      ids' = map toGLuint    $ indices   geo

initVAO :: [Vertex4 Double] -> [TexCoord3 Double] -> [GLuint] -> IO [GLfloat]
initVAO ps ts idx =
  return $ concat $
    fmap (\i -> (\(Vertex4 x y z w) -> fmap realToFrac [x, y, z, w]) (ps!!(fromIntegral (idx!!i))) ++
                (\(TexCoord3 u v w) -> fmap realToFrac [u, v, w])    (ts!!(fromIntegral       i))) iter
      where
        iter = [0..(length idx)-1]

indexedVAO :: [Vertex4 Double] -> [TexCoord3 Double] -> [GLuint] -> Int -> IO ([GLfloat],[GLuint])
indexedVAO ps ts is st =
  do
    vs <- initVAO ps ts is
    let iListSet = indexedListSet vs st
        iList    = (indexed $ chunksOf st vs)
        idx = fmap (\(i,_) -> (fromIntegral i)) (matchLists iListSet iList)
        vx  = concat $ fmap (\x -> snd x) iListSet
    return (vx, idx)

-- instance Drawables Geo where
--   toDrawable :: Geo -> IO Drawable
--   toDrawable geo =
--     do
--       let ps  = map toVertex4   $ positions geo
--           uvs = map toTexCoord3 $ uv geo
--           ids = map toGLuint    $ indices geo
--       return $ Drawable ps uvs ids

-- initVAO :: [Vertex4 Double] -> [TexCoord3 Double] -> [GLuint] -> IO [GLfloat]
-- initVAO ps ts idx =
--   return $ concat $
--     fmap (\i -> (\(Vertex4 x y z w) -> fmap realToFrac [x, y, z, w]) (ps!!(fromIntegral (idx!!i))) Prelude.++
--                 (\(TexCoord3 u v w) -> fmap realToFrac [u, v, w])    (ts!!(fromIntegral       i))) iter
--       where
--         iter = [0..(length idx)-1]

-- TODO: turn arguments into Vector type, then toList will work too
-- Data.Vector toList fromList etc.

-- initVAO' :: [Vertex4 Double] -> [TexCoord3 Double] -> [GLuint] -> [GLfloat]
-- DV.fromList $ fmap (\(x,y,z) -> (x,y,z,0.0::Double)) $ position geo
initVAO' :: Geo -> [GLfloat]
initVAO' geo = result
  where
    ps'  = DV.fromList $ fmap (\(x,y,z) -> (x,y,z,0.0::Double)) $ positions geo
    ts'  = DV.fromList $ uv geo
    idx' = DV.fromList $ fmap (\x -> unsafeCoerce x :: Int) $ indices geo
    idx''= [0..(length idx')-1]
    ts'' = DV.fromList $ fmap (\idx -> ts' ! idx) idx''
    result =  fmap realToFrac . concat $
      DV.zipWith (\id t -> (\(x,y,z,w) -> [x,y,z,w])(ps' ! id) ++ (\(x,y,z) -> [x,y,z])(t) ) idx' ts''

indexedVAO' :: [Vertex4 Double] -> [TexCoord3 Double] -> [GLuint] -> Int -> Geo -> IO ([GLfloat],[GLuint])
indexedVAO' ps ts is st geo =
  do
    let vs = initVAO' geo
        iListSet = indexedListSet vs st
        iList    = (indexed $ chunksOf st vs)
        idx = fmap (\(i,_) -> (fromIntegral i)) (matchLists iListSet iList)
        vx  = concat $ fmap (\x -> snd x) iListSet
    return (vx, idx)

-- indexedVAO :: [Vertex4 Double] -> [TexCoord3 Double] -> [GLuint] -> Int -> Geo -> IO ([GLfloat],[GLuint])
-- indexedVAO ps ts is st geo =
--   do
--     vs <- initVAO ps ts is
--     let iListSet = indexedListSet vs st
--         iList    = (indexed $ chunksOf st vs)
--         idx = fmap (\(i,_) -> (fromIntegral i)) (matchLists iListSet iList)
--         vx  = concat $ fmap (\x -> snd x) iListSet
--     return (vx, idx)

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
               Nothing  -> (-iy, cy) ) nili -- | if a unique index is found - flip the sign
                                            -- | the idea is to separate normal indexes
                                            -- | and unique indexes -> [idx:uidx] later
      where
        nili = elemIndex cy cxs
        cxs  = fmap (\(i,s) -> s) il :: [[GLfloat]]
    
            -- :: initVAO   -> Stride -> indexed VAO
indexedListSet :: [GLfloat] -> Int -> [(Int,[GLfloat])]
indexedListSet vao n =
  fmap (\x -> x) $ indexed $ DS.toList $ DS.fromList $ chunksOf n $ vao

-- jsonFile :: FilePath
-- jsonFile = "./model.pgeo"

main :: IO ()
main = do
  args <- getArgs
  let fileIn  =  (unsafeCoerce (args!!0) :: FilePath)
      fileOut =  (unsafeCoerce (args!!1) :: FilePath)

  -- geo <- readPGeo jsonFile
  geo <- readPGeo fileIn
  (Drawable vs idx) <- toDrawable geo -- $ (geometry . object) game
  -- drw <- toDrawable geo
  -- let stride = 7; ps = (verts drw); ts = (uv drw); idx = (ids drw);
  --(vs, idx) <- indexedVAO ps ts idx stride geo
  -- (vs, idx') <- indexedVAO' ps ts idx stride geo
  I.writeFile fileOut (encodeToLazyText (vs, idx))
