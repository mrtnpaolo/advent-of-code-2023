module Main (main) where

import Advent    (getInputLines,count)
import Data.List (transpose)

main =
  do inp <- getInputLines (tail . words) 6
     print (part1 (parse1 inp))
     print (part2 (parse2 inp))
  where
    parse1 = transpose . map (map (read @Float))
    parse2 =             map (read @Float . concat)

wins [ms,mm] = floor ((ms + δ) / 2) - ceiling ((ms - δ) / 2) + 1
  where
    δ = sqrt (ms^2 - 4*mm - 2)

part1 = product . map wins

part2 = wins
