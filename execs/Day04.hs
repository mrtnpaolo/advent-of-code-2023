module Main (main) where

import Advent          (getInputLines,count)
import Data.List.Split (splitOn)

main =
  do inp <- getInputLines parse 4
     print (part1 inp)
     print (part2 inp)
  where
    parse (tail . splitOn ": " -> [nums]) = count (`elem` winning) entries
      where
        [winning,entries] = map (read @Int) . words <$> splitOn "|" nums

part1 = sum . map score

score 0 = 0
score n = 2^(n-1)

part2 = sum . foldr (\wins rest -> 1 + sum (take wins rest) : rest) []
