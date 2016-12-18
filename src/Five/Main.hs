{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow
import           Crypto.Hash.MD5       as MD5
import qualified Data.ByteString       as B hiding (foldl')
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Data.Char             (ord)
import           Data.List             (group, groupBy, nub, sort)
import           GHC.Exts              (groupWith, sortWith, the)
import           Lib                   (showHash)
import           Numeric               (showHex)

{- The eight-character password for the door is generated one character at a time by finding the MD5 hash of
some Door ID (your puzzle input) and an increasing integer index (starting with 0).

A hash indicates the next character in the password if its hexadecimal representation starts with five
zeroes. If it does, the sixth character in the hash is the next character of the password. -}

input :: B.ByteString
input = "uqwqemis"

main = do
  print $ map snd . take 8 $ filter fst hashes1
  print $ uglyhack (concat $ take 15 $ groupBy (\x y -> fst x == fst y) $ (fmap snd) $ filter fst hashes2)
    where uglyhack horrible = map snd $ take 8 $ sort $ foldl (flip mix) [] horrible
  -- all id (elem <$> ['0'..'7'] <*> (last $ takeWhile (\x -> length x < 8) (scanl (flip mix) [] broke)))

nums :: [B.ByteString]
nums = (BC.pack . show) <$> [0..]

-- hashes :: [(Bool, Char)]
hashes = MD5.hash <$> (zipWith B.append (repeat input) nums)
hashes1 = fmap isNextChar hashes
hashes2 = fmap isNextChar' hashes

isNextChar  = (all (== '0') . take 5  &&& (!! 5) ) . showHash . B.take 7
isNextChar' = (all (== '0') . take 5 &&& ((!! 5) &&& (!! 6))) . showHash . B.take 7

mix (p,c) []  = (p,c):[]
mix (p,c) pcs = if not $ p `elem` map fst pcs then (p,c):pcs else pcs
