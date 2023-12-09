module Main (main) where

import Advent (getInputLines)

main =
  do inp <- getInputLines (map (read @Int) . words) 9
     print (part1 inp)
     print (part2 inp)

part1 = sum . map predict

predict = sum . map last . takeWhile (not . all (0==)) . iterate diffs

diffs xs = zipWith (-) (tail xs) xs

part2 = part1 . map reverse
