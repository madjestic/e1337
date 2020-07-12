module Main where

import Data.Aeson                (decodeFileStrict)
import Data.Text.Lazy            (Text)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Text.Lazy.IO as IO
import Data.List.Extra           (sortOn, groupOn)


-- fib n = fibs!!n
-- fibs  = 0 : 1 : zipWith (+) fibs (tail fibs)

shuffle :: [Int] -> [[Int]]
shuffle x =
  (fmap . fmap) snd $ groupOn fst $ sortOn fst $ zip x [0..]


readTempFile :: FilePath -> IO [Int]
readTempFile file = 
  do
    d <- decodeFileStrict file :: IO (Maybe ([Int]))
    return $ case d of
               Just d  -> d
               Nothing -> []


main :: IO ()
main =
  do
    d <- readTempFile "./resources/rpcShuffler/.p2h" 

    let result = (shuffle d) :: [[Int]]
        fileOut =  ( "./resources/rpcShuffler/.h2p" :: FilePath)
        
    IO.writeFile fileOut (encodeToLazyText (result))  
