module Main (main) where

import Advent          (getInput)
import Data.List       (foldl1')
import Data.List.Split (chunksOf)
import Data.Map.Strict (fromList,(!),keys)

main =
  do (steps,m) <- getInput (go . parse) 8
     print (part1 steps)
     print (part2 steps m)
  where
    clean c | c `elem` "=(,)" = ' ' | otherwise = c
    parse (words . map clean -> (dirs:(chunksOf 3 -> nodes))) =
      (dirs,fromList [(n,(l,r))| [n,l,r] <- nodes])

go (dirs,m) = (steps 0 (cycle dirs),m)
  where
    move 'L' = fst
    move 'R' = snd
    steps n _      [_,_,'Z'] = n
    steps n (d:ds) node      = steps (n+1) ds (move d (m ! node))

part1 steps = steps "AAA"

part2 steps m = foldl1' lcm [steps node | node@[_,_,'A'] <- keys m]
