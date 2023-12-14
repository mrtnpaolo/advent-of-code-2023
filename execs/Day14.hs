module Main (main) where

import Advent   (getInputMap,Coord(..),above,left,below,right,findRepeatOn)
import Data.Ix  (inRange)
import Data.Ord (Down(..))
import Data.List       qualified as L
import Data.Map.Strict qualified as M

main =
  do m <- getInputMap 14
     let ((cm,_),(cM,_)) = (M.findMin m,M.findMax m); inside = inRange (cm,cM)
         lever dir m   = L.foldl' (shift dir) m (rounds dir [ c | (c,'O') <- M.assocs m ])
         shift dir m c = M.insert c' 'O' (M.insert c '.' m)
           where
             c' = last (takeWhile valid (iterate (move dir) c))
             valid d = inside d && (d == c || m M.! d == '.')
     print (part1 lever m)
     print (part2 lever m)

part1 lever = load . lever N

rounds N = id; rounds W = id; rounds S = L.sortOn Down; rounds E = L.sortOn Down

move N = above; move W = left; move S = below; move E = right

load m = sum [ yM - y | (C y _,'O') <- M.assocs m ]
  where
    yM = 1 + maximum [ y | C _ y <- M.keys m ]

part2 lever m = load (ms !! (first + i))
  where
    ms = iterate (lever E . lever S . lever W . lever N) m
    (first,next) = findRepeatOn (M.keys . M.filter ('O'==)) ms
    i = (1_000_000_000 - first) `mod` (next - first)
