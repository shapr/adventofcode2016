module Main where

import           Data.Monoid

input = ".^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^."

isTrap "^^." = '^' -- Its left and center tiles are traps, but its right tile is not.
isTrap ".^^" = '^' -- Its center and right tiles are traps, but its left tile is not.
isTrap "^.." = '^' -- Only its left tile is a trap.
isTrap "..^" = '^' -- Only its right tile is a trap.
isTrap _     = '.' -- None of the above, tile is safe

walk (a:b:c:ds) = isTrap [a,b,c] : walk (b:c:ds)
walk _          = []

inflate input = "." <> input <> "." -- add safe tiles on either end

nextRow thisRow = walk $ inflate thisRow

main = do print $ length $ filter (== '.') (concat $ take 40 rows)
          print $ length $ filter (== '.') (concat $ take 400000 rows)
  where rows = iterate nextRow input
