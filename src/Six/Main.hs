module Main where

import           Data.List

byLength a b = if length a < length b then GT else LT

chunk  :: Ord a => [a] -> [[a]]
chunk = sortBy byLength . group . sort

freq f s = concatMap nub $ concatMap (take 1 . f) $ (fmap . fmap) chunk (transpose . lines) s

main = do contents <- readFile "input.txt"
          print $ freq id contents
          print $ freq reverse contents
