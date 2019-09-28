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
import Drawable
import Shape2D
import Rendering

main :: IO ()
main = do
  args <- getArgs
  let fileIn  =  (unsafeCoerce (args!!0) :: FilePath)
      fileOut =  (unsafeCoerce (args!!1) :: FilePath)

  geo <- readPGeo fileIn
  (Drawable vs idx) <- toDrawable geo -- $ (geometry . object) game
  I.writeFile fileOut (encodeToLazyText (vs, idx))
