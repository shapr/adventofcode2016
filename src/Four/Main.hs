module Main where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Ord

{-
Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a checksum in square brackets.
A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with ties broken by alphabetization.
What is the sum of the sector IDs of the real rooms?

First sort the name by letter frequency, then subsort by alphabet
or simpler, sort by alphabet, then group, then sort by size, assuming a stable sort
-}

yes = ["aaaaa-bbb-z-y-x-123[abxyz]","a-b-c-d-e-f-g-h-987[abcde]","not-a-real-room-404[oarel]"]
no  = ["totally-real-room-200[decoy]"]

-- this could use a real parser
parse room = (sort $ filter isAlpha name,sectorid,chksum)
  where name = takeWhile (not . isDigit) room
        sectorid = takeWhile isDigit  (dropWhile (not . isDigit) room)
        chksum = tail $ takeWhile (/= ']') (dropWhile (/= '[') room)

makechksum :: Ord a => [a] -> [[a]]
makechksum = sortBy byLength . group . sort

byLength a b = if length a < length b then GT else LT

check (roomname,sectorid,checksum) = thissum == checksum
  where triple = parse roomname
        thissum = concatMap (take 1) (take 5 . makechksum $ first triple)

main = do contents <- readFile "input.txt"
          print $ sum $ map (read . second) $ filter check (map parse $ lines contents)


first (a,_,_) = a
second (_,b,_) = b
third (_,_,c) = c
