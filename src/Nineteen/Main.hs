module Main where

import           Data.List (sort)

everyOther []       rs = rs
everyOther (a:[])   rs = a:rs
everyOther (a:b:cs) rs = everyOther cs (a:rs)

numberOfElves = 3017957

eo ns = if (length ns) `mod` 2 == 0 then dropped else (last dropped) : (init dropped)
  where dropped = reverse $ everyOther ns []

main = print $ head . concat . take 1 . drop 49 $ iterate eo [1.. numberOfElves]
