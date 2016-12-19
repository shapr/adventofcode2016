module Lib where

import qualified Data.ByteString as B
import           Data.Char       (ord)


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
