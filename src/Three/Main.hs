module Main where

import           Control.Applicative
import           Data.List

-- part one
triangles :: String -> [[Int]]
triangles t = (sort . fmap read ) <$> fmap words (lines t)

istriangle (a:b:c:[]) =  a + b > c

main = do contents <- readFile "input.txt"
          print $ length (filter istriangle (triangles contents))
