{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputLines parse 4
     print (part1 inp)
     print (part2 inp)
  where
    parse (L.splitOn ": " -> [words -> ["Card",read @Int -> n],rest]) = (n,count (`elem` wins) nums)
      where
        [wins,nums] = map (map (read @Int) . words) $ L.splitOn "|" rest

part1 = sum . map score

score (i,n) =
  case n of
    0 -> 0
    n -> 2^(n-1)

part2 = sum . L.foldl' add M.empty

add m (i,n) = M.unionWith (+) m' $ M.fromList [ (j,n') | j <- [i+1..i+n] ]
  where
    m' = M.insertWith (+) i 1 m
    n' = m' M.! i
