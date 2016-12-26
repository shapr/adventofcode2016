module Lib where

import           Control.Exception
import qualified Data.ByteString        as B
import           Data.Char              (ord)
import           Text.Megaparsec
import           Text.Megaparsec.Lexer  (integer)
import           Text.Megaparsec.String

showHash :: B.ByteString -> String
showHash = fmap (toEnum.fromEnum) . hexalise . B.unpack

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

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal [] = []
replaceNth n newVal (x:xs)
  | n < 0 = error "replaceNth got a value less than zero, NO"
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

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
