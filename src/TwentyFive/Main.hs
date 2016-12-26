{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Data.Maybe             (maybe)
import           Data.Sequence          hiding (drop, filter, length, reverse,
                                         take, zipWith)
import           Lib
import           Text.Megaparsec
import           Text.Megaparsec.String


data AState = S { _a :: !Int, _b :: !Int, _c :: !Int, _d :: !Int, _pc :: !Int, _tglm :: Maybe Int, _outs :: [Int] } deriving (Show,Eq)
makeLenses ''AState


getOA n = S n 0 0 n 0 Nothing []

main = do contents <- readFile "input.txt" -- non-optimized
          let program = parseLines expr contents
              -- 45 is effectively infinite, right?
          print $ map fst $ take 1 $ filter (\x -> (length . snd) x > 45) (map (\i -> (i,take 50 $ takeWhile id $ zipWith (==) (whoknows i program) yes)) [0..])

-- lazy list of chunks of the correct answer
yes :: [[Int]]
yes = zipWith ($) (map take [0..]) (repeat $ cycle [0,1])

-- chunks of possibly the right thing, for each int
whoknows n p = (\i -> fmap (reverse . getouts . head) $ groupBy outgroup $ (iterate eval'' . (,) p . getOA) i) n

getouts o = view outs $ snd o
outgroup one two = getouts one == getouts two

getlens 'a' = a
getlens 'b' = b
getlens 'c' = c
getlens 'd' = d
getlens  _  = error "you broke getlens"

step as (Jnz (Lit 0)      _)    = over pc (+1) as
step as (Jnz (Lit z) (Lit i))   = if z == 0 then over pc (+1) as else over pc (+ i) as
step as (Jnz (Reg r) (Lit i))   = if view (getlens r) as == 0 then over pc (+1) as else over pc (+ i) as
step as (Jnz (Lit i) (Reg r))   = if i == 0 then over pc (+1) as else over pc (+ view (getlens r) as) as
step as (Cpy (Lit i) (Reg r))   = over pc (+1) $ set (getlens r) i as
step as (Cpy (Lit _) (Lit _))   = as -- can't copy to a *value*
step as (Cpy (Reg r1) (Reg r2)) = over pc (+1) $ set (getlens r2) (view (getlens r1) as) as
step as (Dec (Reg ch))          = over pc (+1) $ over (getlens ch) (subtract 1) as
step as (Inc (Reg ch))          = over pc (+1) $ over (getlens ch) (+1) as
step as (Tgl (Reg ch))          = over pc (+1) $ set tglm (Just (view (getlens ch) as)) as
step as (Out (Reg ch))          = over pc (+1) $ over outs (view (getlens ch) as :) as
step as (Out (Lit i))           = over pc (+1) $ over outs (i:) as
step as ins                     = error ("step function got " ++ show as ++ " " ++ show ins)

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

womp :: ([Expr],AState) -> Int -> ([Expr],AState)
womp (es, as) i = (replaceNth exactIndex newThing es, set tglm Nothing as)
  where newThing = toggle (es !! exactIndex)
        exactIndex = i + view pc as - 1 -- check out of bounds?

replaceNth i n ls = foldr (:) [] $ update i n $ fromList ls

data Expr = Cpy !Expr !Expr
          | Inc !Expr
          | Dec !Expr
          | Jnz !Expr !Expr
          | Lit !Int
          | Reg !Char
          | Tgl !Expr
          | Out !Expr
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
  string "tgl" >> space
  e <- val
  return $ Tgl e

cpy :: Parser Expr
cpy = do
  string "cpy" >> space
  x <- val
  space
  y <- reg
  return $ Cpy x y

jnz :: Parser Expr
jnz = do
  string "jnz" >> space
  z <- val
  space
  j <- val
  return $ Jnz z j

inc :: Parser Expr
inc = do
  string "inc" >> space
  r <- reg
  return $ Inc r

out :: Parser Expr
out = do
  string "out" >> space
  v <- val
  return $ Out v

dec :: Parser Expr
dec = do
  string "dec" >> space
  r <- reg
  return $ Dec r

expr :: Parser Expr
expr = cpy <|> jnz <|> inc <|> dec <|> tgl <|> out
