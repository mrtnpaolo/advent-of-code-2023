module Main (main) where

import Advent             (getInputArray,count)
import Advent.Search      (dfsOn,bfs)
import Advent.Coord
import Data.Ix            (range)
import Data.Set           qualified as S
import Data.Array.Unboxed qualified as A

main =
  do a <- getInputArray 10
     let l = loop a
     print (part1 l)
     print (part2 l a)

part1 = (`div` 2) . length

a @@ c = case a A.!? c of Just x -> x; Nothing -> '.'

loop a = dfsOn fst next begin
  where
    next ((a @@) -> 'S',_) = []
    next (c,f) = [p]
      where
        p =
          case (a @@ c,f) of
            ('|',South) -> (above c,south); ('|',North) -> (below c,north)
            ('-',West ) -> (right c,west ); ('-',East ) -> (left  c,east )
            ('L',North) -> (right c,west ); ('L',East ) -> (above c,south)
            ('J',North) -> (left  c,east ); ('J',West ) -> (above c,south)
            ('7',South) -> (left  c,east ); ('7',West ) -> (below c,north)
            ('F',South) -> (right c,west ); ('F',East ) -> (below c,north)

    [start] = [ c | (c,'S') <- A.assocs a ]

    begin = head $
      [ (c,south) | c <- [above start], a @@ c `elem` "|7F" ] ++
      [ (c,west ) | c <- [right start], a @@ c `elem` "-J7" ] ++
      [ (c,north) | c <- [below start], a @@ c `elem` "|LJ" ] ++
      [ (c,east ) | c <- [left  start], a @@ c `elem` "-LF" ]

part2 l a = count (`S.notMember` s) b
  where
    s = S.fromList (map fst l)
    (C ym xm,C yM xM) = A.bounds a
    q = (yM-ym) `div` 4
    b = range (C (ym+q+1) (xm+q+1),C (yM-q) (xM-q))
