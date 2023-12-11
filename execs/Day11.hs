module Main (main) where

import Advent    (getInput,count,withCoords,Coord(..),addCoord,scaleCoord,manhattan)
import Data.List (findIndices,transpose,tails)

main =
  do inp <- getInput parse 11
     print (part1 inp)
     print (part2 inp)

parse (lines -> inp) = expand
  where
    m        = withCoords id inp
    [ys,xs]  = findIndices (all ('.'==)) <$> [inp,transpose inp]
    expand n = [ c `addCoord` scaleCoord (n-1) (C (count (< y) ys) (count (< x) xs))
               | (c@(C y x),'#') <- m ]

part1 expand = sum . dists . expand $ 2

part2 expand = sum . dists . expand $ 1_000_000

dists gs = [ manhattan a b | (a:bs) <- tails gs, b <- bs ]
