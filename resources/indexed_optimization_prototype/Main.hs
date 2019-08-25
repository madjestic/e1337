module Main where

import Data.List (elemIndex, sortBy, sort)
import Data.Function (on)

-- full non-indexed list of indexes : niloi
-- [(0,"a"), (1, "b"), (2,"c"), (3, "b")]
-- a list of indices                : loi
-- [(0,"a"), (1, "b"), (2,"c")]
-- turn into:
-- [(0,"a"), (1, "b"), (2,"c"), (1, "b")] -> [0,1,2,1]

-- iList - indexed list (a list of indices)

iList  = [(0,"a"), (1, "b"), (2,"c")]
niList = [(0,"a"), (1, "b"), (2,"c"), (3, "b"), (4, "d"), (5, "e")]

-- | matchLists - cross-match 2 listst, replacing the elements of list2 with matching
-- |          with elements of list1, concatenating the non-matched elements.

-- |   il - indexed list
-- |  nil - non-indexed list
matchLists :: [(Integer, [Char])] -> [(Integer, [Char])] -> [(Integer, [Char])]
matchLists il nil =
  fmap (mFunc il ) nil -- | mFunc - matching function
  where
    -- | il      - indexed list
    -- | nile    - non indexed list element
    -- | max_idx - max index value, all non-indexed elements should be indexed
    -- |           as max_idx + m, where m is the number of elements in il
    -- |   Replaces the target element with the first match from the matching list il
    mFunc il nile@(iy, cy) =
      (\x -> case x of
               Just idx -> il!!idx
               Nothing  -> (-iy, cy) ) nili -- | if a unic index is found - flip the sign
                                            -- | the idea is to separate normal indexes
                                            -- | and unique indexes -> [idx:uidx]
      where
        nili = elemIndex cy cxs
        cxs  = fmap (\(i,s) -> s) il :: [[Char]]

sortLists :: [(Integer, [Char])] -> [(Integer, [Char])] -> [(Integer, [Char])]
sortLists il nil =
  pos_idx ++ neg_idx
  where
    -- | uil - unsorted indexed list
    uil            = sortBy (compare `on` fst) (matchLists iList niList)
    pos_idx        = (snd splitListTuple)
    neg_idx        = zipWith (\i x@(ix, cx) -> (i, cx))  [size..]
                     $ reverse (fst splitListTuple)
    size           = toEnum $ length il :: Integer
    splitListTuple = splitAt zero uil
    idx            = elemIndex True $ fmap (\(i,_) -> i > 0) uil
    zero           = case idx of
                       Just i  -> i - 1
                       Nothing -> 0

-- | sortLists iList niList
--   [(0,"a"),(1,"b"),(1,"b"),(2,"c"),(3,"d"),(4,"e")]

main :: IO ()
main = undefined
  -- do
  --   let iList  = [(0,"a"), (1, "b"), (2,"c")]
  --       niList = [(0,"a"), (1, "b"), (2,"c"), (3, "b")]

    
