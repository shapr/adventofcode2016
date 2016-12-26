{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Data.Char
import           Data.Monoid
import           Data.String
import           Lib
import           Text.Megaparsec
import           Text.Megaparsec.String

{-
While snooping around the local network of EBHQ, you compile a list of IP addresses (they're IPv7, of course; IPv6 is much too limited). You'd like to figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA. An ABBA is any four-character sequence which consists of a pair of two different characters followed by the reverse of that pair, such as xyyx or abba. However, the IP also must not have an ABBA within any hypernet sequences, which are contained by square brackets.

For example:

abba[mnop]qrst supports TLS (abba outside square brackets).
abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters must be different).
ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though it's within a larger string).
How many IPs in your puzzle input support TLS?
-}

main = do contents <- readFile "input.txt"
          let addys = parseLines nadd contents
          print $ length $ filter check addys

check :: NAddress -> Bool
check (NA nas) = any check' as && all (not . check') hs
  where hs = filter hypers nas
        as = filter (not . hypers) nas

check' (Ha s) = hasAbba s
check' (Aa s) = hasAbba s

hypers (Ha _) = True
hypers _      = False

hasAbba (a:b:c:d:es) = (a == d && b == c && a /= b) || hasAbba (b:c:d:es)
hasAbba _            = False

data Netaddy = Ha String | Aa String deriving Show -- HyperNet | Address

data NAddress = NA ![Netaddy] deriving Show

nadd :: Parser NAddress
nadd = do as <- some (address' <|> address'')
          return $ NA as

address' :: Parser Netaddy
address' = do
  a <- some letterChar
  return $ Aa a

address'' :: Parser Netaddy
address'' = do char '['
               h <- some letterChar
               char ']'
               return $ Ha h
