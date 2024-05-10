module Main (main) where

import Advent.Input (getInputLines)
import Advent.Coord (Coord(..),origin,north,south,west,east,scaleCoord,addCoord)
import Numeric      (readHex)
import Data.List    (scanl')

main =
  do inp <- getInputLines parse1 18
     print (part1 inp)
     inp <- getInputLines parse2 18
     print (part2 inp)
  where
    parse1 (words -> [[dir -> c],read @Int -> n,_]) = scaleCoord n c
    dir 'U' = north; dir 'D' = south; dir 'L' = west; dir 'R' = east
    parse2 = decode . last . words . filter (`notElem` "(#)")
    decode (readHex -> [((`quotRem` 16) -> (n,r),_)]) = scaleCoord n $ dir ("RDLU" !! r)

part1 = area . polygon

part2 = part1

polygon = scanl' addCoord origin

area p = abs (shoelace p) + perimeter p `div` 2 + 1

shoelace p = sum [ (y + y')*(x - x') | C y x <- p | C y' x' <- tail p ] `div` 2

perimeter p = sum [ abs (y'-y) + abs (x'-x) | C y x <- p | C y' x' <- tail p ]
