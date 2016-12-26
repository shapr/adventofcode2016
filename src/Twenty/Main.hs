module Main where
import           Data.Range.Range
import           Lib
import           Text.Megaparsec
import           Text.Megaparsec.String

main = do contents <- readFile "input.txt"
          let rs = parseLines range contents
          print $ head $ difference [fullrange] rs
          print $ length $ difference [fullrange] rs
            where fullrange = SpanRange 0 4294967295

range :: Parser (Range Int)
range = do n1 <- number
           string "-"
           n2 <- number
           return $ SpanRange n1 n2
