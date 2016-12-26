{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.Tuple
import           Data.List              (foldl')
import           Data.Maybe
import           Data.Sequence          hiding (drop, length, take)
import           Lib                    hiding (replaceNth)
import           Text.Megaparsec
import           Text.Megaparsec.Lexer  (integer)
import           Text.Megaparsec.String


data AState = S { _a :: !Int, _b :: !Int, _c :: !Int, _d :: !Int, _pc :: !Int, _tglm :: Maybe Int } deriving Show
emptyA = S 0 0 0 0 0 Nothing
makeLenses ''AState

test_input = "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a\n"

main = do contents <- readFile "input.txt"
          let program = parseLines expr contents
          -- print $ last $ takeWhile (\as -> view pc as < length program) (iterate (eval program) emptyA)
          print $ last $ takeWhile (\(es,as) -> view pc as < length program) (iterate eval'' (program,emptyA))
          -- print $ last $ takeWhile (\as -> (view pc as) < length program) (iterate (eval program) (S 0 0 1 0 0))


program = [Cpy (Lit 2) (Reg 'a'),Tgl (Reg 'a'),Tgl (Reg 'a'),Tgl (Reg 'a'),Cpy (Lit 1) (Reg 'a'),Dec (Reg 'a'),Dec (Reg 'a')]
program' = [Cpy (Lit 2) (Reg 'a'),Tgl (Reg 'a'),Tgl (Reg 'a'),Tgl (Reg 'a')]

getlens 'a' = a
getlens 'b' = b
getlens 'c' = c
getlens 'd' = d

step as (Jnz (Lit z) (Lit i))   = if z == 0 then over pc (+1) as else over pc (+ i) as
step as (Jnz (Reg r) (Lit i))   = if view (getlens r) as == 0 then over pc (+1) as else over pc (+ i) as
step as (Jnz (Lit i) (Reg r))   = if i == 0 then over pc (+1) as else over pc (+ view (getlens r) as) as
step as (Cpy (Lit i) (Reg r))   = over pc (+1) $ set (getlens r) i as
step as (Cpy (Lit i1) (Lit i2)) = as
step as (Cpy (Reg r1) (Reg r2)) = over pc (+1) $ set (getlens r2) (view (getlens r1) as) as
step as (Dec (Reg ch))          = over pc (+1) $ over (getlens ch) (subtract 1) as
step as (Inc (Reg ch))          = over pc (+1) $ over (getlens ch) (+1) as
step as (Tgl (Reg ch))          = over pc (+1) $ set tglm (Just (view (getlens ch) as)) as -- ARGH
{-
For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
The arguments of a toggled instruction are not affected.
If an attempt is made to toggle an instruction outside the program, nothing happens.
If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed until the next time it is reached.
-}

toggle :: Expr -> Expr
toggle (Inc e)     = Dec e
toggle (Tgl e)     = Inc e
toggle (Jnz e1 e2) = Cpy e1 e2
toggle (Cpy e1 e2) = Jnz e1 e2
toggle (Dec e)     = Inc e

eval :: [Expr] -> AState -> AState
eval es as = step as (es !! view pc as)

eval'' (es, as) = maybe (es,res) (womp (es,res)) (view tglm res)
  where res = eval es as

womp :: ([Expr],AState) -> Int -> ([Expr],AState) -- this part seems to be working
womp (es, as) i = (replaceNth exactIndex newThing es, set tglm Nothing as)
  where newThing = toggle (es !! exactIndex)
        exactIndex = (i + (view pc as) - 1) -- check out of bounds?

replaceNth i n ls = foldr (:) [] $ update i n $ fromList ls

data Expr = Cpy !Expr !Expr
          | Inc !Expr
          | Dec !Expr
          | Jnz !Expr !Expr
          | Lit !Int
          | Reg !Char
          | Tgl !Expr -- Reg or Literal?
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

tgl :: Parser Expr
tgl = do
  string "tgl"
  space
  e <- val
  return $ Tgl e

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
expr = cpy <|> jnz <|> inc <|> dec <|> tgl
