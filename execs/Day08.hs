module Main (main) where

import Advent          (getInput)
import Data.List       (foldl1)
import Data.List.Split (chunksOf)
import Data.Map.Strict (fromList,(!),keys)

main =
  do (len,m) <- getInput (navigate . parse . map clean) 8
     print (part1 len)
     print (part2 len m)
  where
    clean c | c `elem` "=(,)" = ' ' | otherwise = c
    parse (words -> (dirs:(chunksOf 3 -> maps))) =
      (cycle dirs,fromList [(from,(l,r))| [from,l,r] <- maps])

navigate (dirs,m) = (len 0 dirs,m)
  where
    move 'L' = fst; move 'R' = snd
    len n (d:ds) end node | end node  = n
                          | otherwise = len (n+1) ds end (move d (m ! node))

part1 len = len ("ZZZ"==) "AAA"

part2 len m = foldl1 lcm [len (('Z'==) . last) node | node@[_,_,'A'] <- keys m]
