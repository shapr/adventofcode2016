module Lib where

import qualified Data.ByteString        as B
import           Data.Char              (ord)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Lexer  (integer)
import           Text.Megaparsec.String

showHash :: B.ByteString -> String
showHash = fmap (toEnum.fromEnum) . hexalise . B.unpack

-- hexalise :: [Word8] -> [Word8]
hexalise = concatMap (\c -> [ hex $ c `div` 16, hex $ c `mod` 16 ])
        where hex i
                | i >= 0 && i <= 9   = fromIntegral (ord '0') + i
                | i >= 10 && i <= 15 = fromIntegral (ord 'a') + i - 10
                | otherwise          = 0

-- splitBy :: (a -> Bool) -> [a] -> [[a]]
-- splitBy f (a:as) = if

first (a,_,_) = a
second (_,b,_) = b
third (_,_,c) = c

-- from https://wiki.haskell.org/List_function_suggestions
groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _   []          =  []
groupBy rel (x:xs)      =  (x:ys) : groupBy rel zs
  where (ys,zs) = groupByAux x xs
        groupByAux x0 (x:xs) | rel x0 x = (x:ys, zs)
          where (ys,zs) = groupByAux x xs
        groupByAux y xs = ([], xs)

-- wtf
prs p = parse p ""

prs' p s = runParser' p (initialState s)

-- stolen from glguy - https://github.com/glguy/advent2016/

number :: Num a => Parser a
number =
   char '-' *> (negate <$> number) <|>
   fromInteger <$> integer

parseOrDie :: Parser a -> String -> a
parseOrDie p str =
  case parse p str str of
    Left e   -> error (parseErrorPretty e)
    Right xs -> xs

parseLines :: Parser a -> String -> [a]
parseLines p = parseOrDie (many (p <* eol) <* eof)

wholestring :: String -> Parser String
wholestring = try . string
