module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputLines parse1 18
     print (part1 inp)
     inp <- getInputLines parse2 18
     print (part2 inp)
  where
    parse1 (words -> [[dir -> c],read @Int -> n,_]) = scaleCoord n c

    dir 'U' = north; dir 'D' = south; dir 'L' = west; dir 'R' = east

    parse2 = decode . last . words . filter (`notElem` "(#)")

    decode xs = scaleCoord n c
      where
        c = dir ("RDLU" !! digitToInt (last xs))
        [(n,_)] = readHex (init xs)

part1 = area . polygon

part2 = part1

polygon = L.scanl' addCoord origin

area p = shoelace p + perimeter p `div` 2 + 1

perimeter p = sum [ abs (y'-y) + abs (x'-x) | (C y x,C y' x') <- zip p (tail p) ]

shoelace [_] = 0
shoelace (C y x : xs@(C y' x' : _)) = (y + y') * (x - x') `div` 2 + shoelace xs
