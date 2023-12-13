module Main (main) where

import Advent          (getInput)
import Data.List       (transpose,inits,tails)
import Data.List.Split (splitOn)

main =
  do inp <- getInput parse 13
     print (part1 inp)
     print (part2 inp)
  where
    parse = map lines . splitOn "\n\n"

part1 = sum . map (summary 0)

part2 = sum . map (summary 1)

summary n xs = sum (map (100*) (refls n xs) ++ refls n (transpose xs))

refls n xs = [ i | (i,l,r) <- init (tail (zip3 [0..] (inits xs) (tails xs)))
                 , n == sum (zipWith δ (reverse l) r) ]

δ xs ys = sum (zipWith (\cases a b | a == b -> 0 | otherwise -> 1) xs ys)

