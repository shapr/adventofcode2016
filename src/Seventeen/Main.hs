{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Crypto.Hash.MD5 as MD5
import           Data.Array
import qualified Data.ByteString as B
import           Data.List       (foldl', nub, scanl', sort, sortOn)
import           Data.Monoid
import           Data.Tree
import           Lib             (showHash, third)

{- Only the first four characters of the hash are used;
they represent, respectively, the doors up, down, left, and right from your current position. -}

input :: B.ByteString
input = "vkjiggvb"

getOpenDoors here = isOpen <$> [u,d,l,r]
  where [u,d,l,r] = take 4 (showHash $ MD5.hash here)
        isOpen = flip elem ("bcdef" :: [Char])

possibleMoves input = fmap snd $ filter fst $ zip (getOpenDoors input) "UDLR"
legalMoves (0,3,input) = []
legalMoves (x,y,input) = filter isLegal $ map (moveToPos (x,y) input) $ possibleMoves input

-- 0,0 to 3,3
isLegal (x,y,i) = x > -1 && x < 4 && y > -1 && y < 4

moveToPos (x,y) i 'U' = (x+1,y, B.append i "U")
moveToPos (x,y) i 'D' = (x-1,y, B.append i "D")
moveToPos (x,y) i 'L' = (x,y-1, B.append i "L")
moveToPos (x,y) i 'R' = (x,y+1, B.append i "R")
moveToPos _      _  i = error "bad input to moveToPos"

main = do print $ B.drop (B.length input) $ (third . head) $ take 1 solution
          mapM_ print $ scanl' longer "" (third <$> solution) -- THIS WORKS, but why doesn't it terminate?
            where longer x y = if B.length x > B.length y then x else y

isDone (x,y,i) = x == 0 && y == 3

steps = iterate (concatMap legalMoves) [(3,0,input)]

solution = filter isDone $ concat steps
