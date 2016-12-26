{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.Tuple
import           Lib
import           Text.Megaparsec
import           Text.Megaparsec.Lexer  (integer)
import           Text.Megaparsec.String


data AState = S { _a :: Int, _b :: Int, _c :: Int, _d :: Int, _pc :: Int } deriving Show

makeLenses ''AState

main = do contents <- readFile "input.txt"
          let program = parseLines expr contents
          print $ last $ takeWhile (\as -> view pc as < length program) (iterate (eval program) (S 0 0 0 0 0))
          print $ last $ takeWhile (\as -> view pc as < length program) (iterate (eval program) (S 0 0 1 0 0))

getlens 'a' = a
getlens 'b' = b
getlens 'c' = c
getlens 'd' = d

step as (Jnz (Lit z) (Lit i)) = if z == 0 then over pc (+1) as else over pc (+ i) as
step as (Jnz (Reg r) (Lit i)) = if view (getlens r) as == 0 then over pc (+1) as else over pc (+ i) as
step as (Cpy (Lit i) (Reg r))   = over pc (+1) $ set (getlens r) i as
step as (Cpy (Reg r1) (Reg r2)) = over pc (+1) $ set (getlens r2) (view (getlens r1) as) as
step as (Dec (Reg ch)) = over pc (+1) $ over (getlens ch) (subtract 1) as
step as (Inc (Reg ch)) = over pc (+1) $ over (getlens ch) (+1) as

eval es as = step as (es !! view pc as)

data Expr = Cpy Expr Expr
          | Inc Expr
          | Dec Expr
          | Jnz Expr Expr
          | Lit Int
          | Reg Char
            deriving Show

lit :: Parser Expr
lit = do
  l <- number
  return $ Lit l

val :: Parser Expr
val = lit <|> reg

reg :: Parser Expr
reg = do
  r <- oneOf "abcd"
  return $ Reg r

cpy :: Parser Expr
cpy = do
  string "cpy"
  space
  x <- val
  space
  y <- reg
  return $ Cpy x y

jnz :: Parser Expr
jnz = do
  string "jnz"
  space
  z <- val
  space
  j <- val
  return $ Jnz z j

inc :: Parser Expr
inc = do
  string "inc"
  space
  r <- reg
  return $ Inc r

dec :: Parser Expr
dec = do
  string "dec"
  space
  r <- reg
  return $ Dec r

expr :: Parser Expr
expr = cpy <|> jnz <|> inc <|> dec
