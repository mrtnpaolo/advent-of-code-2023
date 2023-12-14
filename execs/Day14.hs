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
  do inp <- getInputMap 14
     putStrLn (drawCoords inp)
     let m' = part1 inp
     putStrLn (drawCoords m')
     print (load m')
     print (part2 inp)

part1 = lever N

lever _ m = rounds'
  where
    (cm,_) = M.findMin m
    (cM,_) = M.findMax m
    ground = [ (c,'.') | c <- range (cm,cM) ]
    cubes  = [ (c,'#') | (c,'#') <- M.assocs m ]
    m' = M.fromList cubes `M.union` M.fromList ground
    rounds = [ c | (c,'O') <- M.assocs m ]
    rounds' = L.foldl' f m' rounds

f m c = M.insert c' 'O' m
  where
    (C ym _,_) = M.findMin m
    ds = takeWhile (\(C y _) -> ym <= y) (iterate above c)
    c' = last $ takeWhile (\d -> d == c || (m M.! d /= '#' && m M.! d /= 'O')) ds

load m = sum [ yM - y | (C y _,'O') <- M.assocs m ]
  where
    yM = 1 + maximum [ y | C _ y <- M.keys m ]

part2 = const ()
