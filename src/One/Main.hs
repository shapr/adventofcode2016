module Main where

import           Data.List.Split
import           Data.Map.Strict hiding (foldl, map)

input = "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4"

inputToTurnInt (a:as) = case a of 'R' -> (R, (read as :: Int))
                                  'L' -> (L, (read as :: Int))

directions = map inputToTurnInt (splitOn ", " input)

-- convert left and right to exact instead of relative
rToE o []         = []
rToE o ((R,i):as) = (right o, i) : rToE (right o) as
rToE o ((L,i):as) = (left o, i)  : rToE (left o)  as

exact = rToE North directions

-- assuming N, E, S, W, add up all the values
addDir :: (Orientation,Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addDir (North,i) (n,e,s,w) = (n+i,e,s,w)
addDir (East, i) (n,e,s,w) = (n,e+i,s,w)
addDir (South,i) (n,e,s,w) = (n,e,s+i,w)
addDir (West, i) (n,e,s,w) = (n,e,s,w+i)

-- subtract south from north, east from west, give the absolute value of north and east as the vector
-- foldl (flip addDir) (0,0,0,0) exact
total (n,e,s,w) = (abs $ n - s) + (abs $ e - w)

main = print $ total $ foldl (flip addDir) (0,0,0,0) exact

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

-- part two
{-
this part is way more fun,
must produce x,y coords for each step, and then look for the second occurence of a location
-}
