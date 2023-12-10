module Main (main) where

import Advent             (getInputArray,count,dfsOn,Coord(..),above,right,below,left)
import Data.Ix            (range)
import Data.Maybe         (maybeToList)
import Data.Set           (fromList,notMember)
import Data.Array.Unboxed ((!),(!?),assocs,bounds)

main =
  do a <- getInputArray 10
     let l = loop a
     print (part1 l)
     print (part2 l a)

part1 l = length l `div` 2

loop a = dfsOn fst (\(c,d) -> next c (a ! c) d) start
  where
    next _ 'S' _ = []
    next c '|' S = [(above c,S)]; next c '|' N = [(below c,N)]
    next c '-' W = [(right c,W)]; next c '-' E = [(left  c,E)]
    next c 'L' N = [(right c,W)]; next c 'L' E = [(above c,S)]
    next c 'J' N = [(left  c,E)]; next c 'J' W = [(above c,S)]
    next c '7' S = [(left  c,E)]; next c '7' W = [(below c,N)]
    next c 'F' S = [(right c,W)]; next c 'F' E = [(below c,N)]

    start:_ = [ (move c,d)
              | (c,'S') <- assocs a
              , (d,move,ps) <- [(S,above,"|7F"),(W,right,"-J7"),(N,below,"|LJ"),(E,left,"-LF")]
              , x <- maybeToList (a !? move c), x `elem` ps ]

part2 (fromList . map fst -> s) a = count (`notMember` s) b
  where
    (C ym xm,C yM xM) = bounds a
    q = (yM-ym) `div` 4
    b = range (C (ym+q+1) (xm+q+1),C (yM-q) (xM-q))
