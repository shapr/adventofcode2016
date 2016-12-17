module Main where

import           Control.Applicative
import           Data.List

triangles :: String -> [[Int]]
triangles t = fmap read <$> fmap words (lines t)

istriangle (a:b:c:[]) =  a + b > c

main = do contents <- readFile "input.txt"
          print $ length (filter istriangle (sort <$> triangles contents))
          print $ sum $ fmap checkThree $ transpose $ triangles contents

checkThree []         = 0 -- this function is a weird and ugly hack
checkThree (a:b:c:ds) = checkThree ds + if istriangle (sort [a,b,c]) then 1 else 0
