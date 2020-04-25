#!/usr/bin/runhaskell

module Main where

import Material
import Data.Aeson
import System.Directory
import Unsafe.Coerce
import System.Environment        (getArgs)
import Data.Char (toUpper)
import Data.List.Split

-- | This script generates a file structure for a material
--   example:
--   $ ./genMaterial.hs mat/ISS/ISS_AO_04 -- that's formatted i.a.w. the error message from e1337 in case a material is missing
--   > ./Bar/Foo ...

-- TODO: fix so that it works like:
--   $ ./genMaterial.hs dir/file
--   $ splitOn "/" "suka/nah"
--   > ["suka","nah"]

main :: IO ()
main =
  do
    args <- getArgs
    let
      cmdArgs    = splitOn "/" (unsafeCoerce (args!!0) :: String)
      dirName    = cmdArgs!!1
      matName    = cmdArgs!!2
      capitalize = (\x -> (toUpper . head $ x):[] ++ (tail x))

    -- | create a shader dir in ./mat/mymatdir
    putStrLn "Generating dirs..."
    createDirectoryIfMissing True ("./" ++ dirName ++ "/src")

    -- | write  shader file ./mat/mymatdir/mymat
    putStrLn "Generating Materials..."    
    let 
      matSubDirName  = ("mat/" ++ dirName ++ "/src")
      vertShaderPath = ( matSubDirName ++ "/shader.vert")
      fragShaderPath = ( matSubDirName ++ "/shader.frag")
      mat            = Material matName vertShaderPath fragShaderPath []
      in
        do writeMaterial mat ("./" ++ dirName ++ "/" ++ matName)
           copyFile "./default/Default/shader.vert" $ "../" ++ vertShaderPath
           copyFile "./default/Default/shader.frag" $ "../" ++ fragShaderPath
    
    putStrLn "OK"      
