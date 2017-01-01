module Main where

import           Data.Sequence hiding (length, take)
import qualified Data.Sequence as S

main = do print $ getCheckSum 272 "10010000000110000"
          print $ getCheckSum 35651584 "10010000000110000"

getCheckSum n s = doCheck $ getCurve n (S.fromList s)

grow a = (a |> '0') >< (swap <$> (S.reverse a))

swap '1' = '0'
swap '0' = '1'

doCheck s = head . take 1 $ dropWhile (\x -> length x `mod` 2 == 0) $ iterate checksum s

checksum []           = []
checksum ('0':'1':as) = '0' : checksum as
checksum ('1':'0':as) = '0' : checksum as
checksum ('1':'1':as) = '1' : checksum as
checksum ('0':'0':as) = '1' : checksum as
checksum      bi@_    = error ("checksum: bad input "  ++ show bi)

getCurve n i = take n $ foldr (:) [] $ head $ dropWhile (\c -> S.length c < n) (iterate grow i)
