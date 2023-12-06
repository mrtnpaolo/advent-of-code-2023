module Main (main) where

import Advent    (getInput,count)
import Data.List (transpose)

main =
  do inp <- getInput parse 6
     print (part1 (parse1 inp))
     print (part2 (parse2 inp))
  where
    parse  = map (tail . words) . lines
    parse1 = transpose . map (map (read @Int))
    parse2 =             map (read @Int . concat)

wins [tot,len] = count (> len) [ i*t | i <- [1..tot-1], let t = tot-i ]

part1 = product . map wins

part2 = wins
