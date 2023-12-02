module Main (main) where

import Advent          (getInputLines)
import Data.List.Split (splitOneOf, splitOn)
import Data.Map.Strict (fromListWith, (!))

main =
  do inp <- getInputLines parse 2
     print (part1 inp)
     print (part2 inp)

parse xs = (game_id,maximums)
  where
    [words -> ["Game", read @Int -> game_id], splitOneOf ";," -> gs] = splitOn ": " xs
    maximums = fromListWith max [(color,amount) | (words -> [read @Int -> amount,color]) <- gs]

part1 xs = sum [ game_id | (game_id,ms) <- xs, ms!"red" <= 12, ms!"green" <= 13, ms!"blue" <= 14 ]

part2 xs = sum [ product ms | (_,ms) <- xs ]
