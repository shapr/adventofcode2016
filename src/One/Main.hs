module Main where

import           Data.List
import           Data.List.Split
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set hiding (foldl')

-- my puzzle input
input = "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4"

-- handy machinery below
data Orientation = North | East | South | West deriving (Eq, Ord, Show, Enum, Bounded)
data Turn = L | R deriving (Eq, Ord, Show)

class Direction a where
  left :: a -> a
  right :: a -> a
-- there's gotta be a better way, what I really want is an Enum that wraps around, does that exist?

instance Direction Orientation where
  left North = West
  left East  = North
  left South = East
  left West  = South
  right North = East
  right East  = South
  right South = West
  right West  = North

inputToTurnInt (a:as) = case a of 'R' -> (R, read as :: Int)
                                  'L' -> (L, read as :: Int)
                                  _   -> error "bad input for inputToTurnInt"

directions = map inputToTurnInt (splitOn ", " input)

-- convert left and right to exact instead of relative
rToE _ []         = []
rToE o ((R,i):as) = (right o, i) : rToE (right o) as
rToE o ((L,i):as) = (left o, i)  : rToE (left o)  as

exact = rToE North directions

-- assuming N, E, S, W, add up all the values
addDir ::  (Int, Int, Int, Int) -> (Orientation,Int) -> (Int, Int, Int, Int)
addDir (n,e,s,w) (North,i) = (n+i,e,s,w)
addDir (n,e,s,w) (East, i) = (n,e+i,s,w)
addDir (n,e,s,w) (South,i) = (n,e,s+i,w)
addDir (n,e,s,w) (West, i) = (n,e,s,w+i)

-- subtract south from north, east from west, give the absolute value of north and east as the vector
-- foldl (flip addDir) (0,0,0,0) exact
total (n,e,s,w) = abs (n - s) + abs (e - w)

total' (n,e,s,w) = (n - s,e - w,0,0)

main = do print $ total $ foldl' addDir (0,0,0,0) exact
          print $ total $ fromJust $ dup totalDirs



-- part two
{-
this part is way more fun,
must produce x,y coords for each step, and then look for the second occurence of a location
assume we start at 0,0
re-use addDir, but scanl instead of foldl, convert each into x,y then look for the first duplicate
-}

totalDirs :: [(Int, Int, Int, Int)]
totalDirs = map total' $ scanl addDir (0,0,0,0) (concatMap rep exact)

-- stolen from http://stackoverflow.com/a/5438809/39683
dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (z:zs) s = if Set.member z s
                           then Just z
                           else dup' zs (Set.insert z s)

{- the above does not work because I need to check every x,y coordinate that the path crosses,
rather than checking each location where the path turns.
cheesy hack: break each R8 into an exact value of Direction 1, then use the same code above
take 8 $ repeat (Direction 1) -}

rep (East ,i) = replicate i (East ,1)
rep (North,i) = replicate i (North,1)
rep (West ,i) = replicate i (West ,1)
rep (South,i) = replicate i (South,1)
