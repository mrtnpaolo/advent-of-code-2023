module Main (main) where

import Advent          (getInputLines)
import Data.List.Split (splitOn)
import Data.Map.Strict (fromListWith, (!))

main =
  do inp <- getInputLines parse 2
     print (part1 inp)
     print (part2 inp)
  where
    parse xs = (n,m)
      where
        [ words -> ["Game", read @Int -> n],
          concatMap (map words . splitOn ", ") . splitOn "; " -> gs]
          = splitOn ": " xs
        m = fromListWith max [(color,amount) | [read @Int -> amount,color] <- gs]

part1 xs = sum [ n | (n,m) <- xs, m!"red" <= 12 && m!"green" <= 13 && m!"blue" <= 14 ]

part2 xs = sum [ m!"red" * m!"green" * m!"blue" | (_,m) <- xs ]
