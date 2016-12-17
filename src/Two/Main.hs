module Main where

import           Data.List (foldl')

digits = ["UULLULLUULLLURDLDUURRDRRLDURDULLRURDUDULLLUULURURLRDRRRRULDRUULLLLUUDURDULDRRDRUDLRRLDLUDLDDRURURUURRRDDDLLRUDURDULUULLRRULLRULDUDRDRLDLURURUDDUDLURUDUDURLURURRURLUDDRURRDLUURLLRURRDUDLULULUDULDLLRRRDLRDLDUDRDDDRRUURRRRRUURRDRRDLURDRRURDLLUULULLRURDLDDDRRLLRRUURULURUUDDLRRUDDRURUUDLRLRDLRURRRDULLDLRUDDUULRDULURUURDULUDLLRRLDDLRDLRUDRLDDRLRRRDURDULLRRRDRRLUURURDRRDRRLDLUDURURLDUURDRUDRDDRLDRRLDLURURULLUURUDUUDLRLL","LLLULLULDDULRLLURLLLRUUDDLRUULRLULLDLLRRDRLRLRLLDRUUURULDRDDLUDLLDUDULLLRLULLLRULDRDRUDLLRLRLLUDULRRRLDRUULDDULLDULULLUDUDLDRDURDLDLLDUDRRRDLUURRUURULLURLDURLRRLLDDUUULDRLUUDUDLURLULUDURRDRLLDDDDDRRULLRLDULULDDRUURRDLUDDDUDURDDRDRULULLLLUURDURUUUULUDLRURRULRDDRURURLLRLUUDUUURDLLDDLUDRLLLUDLLLLULRLURDRRRDUUDLLDLDDDURRDDRURUURDDRURRLDDDURDLLUURUUULRLUURRUDRLLDLURDUDRLULDLRLULULUDDLRDUDRUDLUULUULDURDRRRRLRULLUDRDDRDLDUDRDRRLDLLLLUDDLRULDLLDDUULDDRRULRRUURUDRDURLLLDDUUDRUUDLULLDR","UDUUULLDDDDLUDLDULRLRDLULLDDRULDURRLURRUDLRRUDURRDUDRRRUULRLLRLUDLDRRDUURDDRDRDUUUDUDLDLLRRLUURLUUUDDDUURLULURRLURRRDRDURURUDRLRUURUDRUDDDRDRDLDRDURDLDRRDUUDLLURLDDURRRLULDRDRLLRLLLRURLDURDRLDRUURRLDLDRLDDDRLDLRLDURURLLLLDDRDUDLRULULLRDDLLUDRDRRLUUULDRLDURURDUDURLLDRRDUULDUUDLLDDRUUULRRULDDUDRDRLRULUUDUURULLDLLURLRRLDDDLLDRRDDRLDDLURRUDURULUDLLLDUDDLDLDLRUDUDRDUDDLDDLDULURDDUDRRUUURLDUURULLRLULUURLLLLDUUDURUUDUULULDRULRLRDULDLLURDLRUUUDDURLLLLDUDRLUUDUDRRURURRDRDDRULDLRLURDLLRRDRUUUURLDRURDUUDLDURUDDLRDDDDURRLRLUDRRDDURDDRLDDLLRR","ULDRUDURUDULLUDUDURLDLLRRULRRULRUDLULLLDRULLDURUULDDURDUUDLRDRUDUDDLDRDLUULRRDLRUULULUUUDUUDDRDRLLULLRRDLRRLUDRLULLUUUUURRDURLLRURRULLLRLURRULRDUURRLDDRRDRLULDDRRDRLULLRDLRRURUDURULRLUDRUDLUDDDUDUDDUDLLRDLLDRURULUDRLRRULRDDDDDRLDLRRLUUDLUURRDURRDLDLDUDRLULLULRLDRDUDLRULLULLRLDDRURLLLRLDDDLLLRURDDDLLUDLDLRLUULLLRULDRRDUDLRRDDULRLLDUURLLLLLDRULDRLLLUURDURRULURLDDLRRUDULUURRLULRDRDDLULULRRURLDLRRRUDURURDURDULURULLRLDD","DURLRRRDRULDLULUDULUURURRLULUDLURURDDURULLRRUUDLRURLDLRUDULDLLRRULLLLRRLRUULDLDLLRDUDLLRLULRLLUUULULRDLDLRRURLUDDRRLUUDDRRUDDRRURLRRULLDDULLLURRULUDLRRRURRULRLLLRULLRRURDRLURULLDULRLLLULLRLRLLLDRRRRDDDDDDULUUDUDULRURDRUDRLUULURDURLURRDRRRRDRRLLLLUDLRRDURURLLULUDDLRLRLRRUURLLURLDUULLRRDURRULRULURLLLRLUURRULLLURDDDRURDUDDULLRULUUUDDRURUUDUURURRDRURDUDRLLRRULURUDLDURLDLRRRRLLUURRLULDDDUUUURUULDLDRLDUDULDRRULDRDULURRUURDU"]

{-
start at 5
1 2 3
4 5 6
7 8 9
g-}
move 'U' here = case here of
  4   -> 1
  5   -> 2
  6   -> 3
  7   -> 4
  8   -> 5
  9   -> 6
  a@_ -> a
move 'L' here = case here of
  2   -> 1
  3   -> 2
  5   -> 4
  6   -> 5
  8   -> 7
  9   -> 8
  a@_ -> a
move 'D' here = case here of
  1   -> 4
  2   -> 5
  3   -> 6
  4   -> 7
  5   -> 8
  6   -> 9
  a@_ -> a
move 'R' here = case here of
  1   -> 2
  2   -> 3
  4   -> 5
  5   -> 6
  7   -> 8
  8   -> 9
  a@_ -> a

{-
part two
    1
  2 3 4
5 6 7 8 9
  A B C
    D
-}

-- this just isn't elegant
move' '1' 'D' = '3'
move' '2' 'R' = '3'
move' '2' 'D' = '6'
move' '3' 'U' = '1'
move' '3' 'L' = '2'
move' '3' 'R' = '4'
move' '3' 'D' = '7'
move' '4' 'L' = '3'
move' '4' 'D' = '8'
move' '5' 'R' = '6'
move' '6' 'U' = '2'
move' '6' 'R' = '7'
move' '6' 'D' = 'A'
move' '7' 'U' = '3'
move' '7' 'R' = '8'
move' '7' 'D' = 'B'
move' '7' 'L' = '6'
move' '8' 'U' = '4'
move' '8' 'R' = '9'
move' '8' 'D' = 'C'
move' '8' 'L' = '7'
move' '9' 'L' = '8'
move' 'A' 'U' = '6'
move' 'A' 'R' = 'B'
move' 'B' 'U' = '7'
move' 'B' 'R' = 'C'
move' 'B' 'D' = 'D'
move' 'B' 'L' = 'A'
move' 'C' 'U' = '8'
move' 'C' 'L' = 'B'
move' 'D' 'U' = 'B'
move' p@_  _  = p

main = do print $ map (foldl' (flip move) 5) digits
          print $ map (foldl' move' '7') digits
