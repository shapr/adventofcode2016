{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow
import           Crypto.Hash.MD5       as MD5
import qualified Data.ByteString       as B hiding (foldl')
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Data.Char             (ord)
import           Numeric               (showHex)

{- The eight-character password for the door is generated one character at a time by finding the MD5 hash of
some Door ID (your puzzle input) and an increasing integer index (starting with 0).

A hash indicates the next character in the password if its hexadecimal representation starts with five
zeroes. If it does, the sixth character in the hash is the next character of the password. -}

input :: B.ByteString
input = "uqwqemis"

main = print $ map snd . take 8 $ filter fst hashes

showHash :: B.ByteString -> String
showHash = map (toEnum.fromEnum) . hexalise . B.unpack

hexalise = concatMap (\c -> [ hex $ c `div` 16, hex $ c `mod` 16 ])
        where hex i
                | i >= 0 && i <= 9   = fromIntegral (ord '0') + i
                | i >= 10 && i <= 15 = fromIntegral (ord 'a') + i - 10
                | otherwise          = 0

nums :: [B.ByteString]
nums = map (BC.pack . show) [0..]

hashes :: [(Bool, Char)]
hashes = map (isNextChar . MD5.hash) (zipWith B.append (repeat input) nums)

isNextChar = (all (== '0') . take 5 . showHash &&& (!! 5) . showHash) . B.take 6
