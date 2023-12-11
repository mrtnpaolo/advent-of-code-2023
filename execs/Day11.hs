module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInput parse 11
     print (part1 inp)
     print (part2 inp)

expand = L.transpose . addrows . L.transpose . addrows

addrows [] = []
addrows (x:xs) | all ('.'==) x = x : x : addrows xs | otherwise = x : addrows xs

makeArray rows =
  A.listArray bounds (concat rows) :: A.UArray Coord Char
    where
      height = length rows
      width  = length (head rows)
      bounds = ( origin, C (height-1) (width-1) )

parse = makeArray . expand . lines

part1 a = sum ps
  where
    ps = L.foldl' f M.empty (L.tails gs)
    gs = [ c | (c,'#') <- A.assocs a ]
    f m [] = m
    f m [_] = m
    f m (x:xs) = m `M.union` (M.mapKeys (\d -> (x,d)) (sp a x xs))

sp a x xs = ds
  where
    ys = bfsOn fst nexts [(x,0)]
    nexts (c,n) = [ (d,n+1) | d <- cardinal c, A.bounds a `inRange` d ]
    ds = M.fromList ys `M.restrictKeys` S.fromList xs



part2 = const ()
