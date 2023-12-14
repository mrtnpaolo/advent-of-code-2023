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
     let m' = lever N inp
         m'' = lever W m'
         m''' = lever S m''
         m'''' = lever E m'''
     putStrLn (drawCoords m')
     putStrLn (drawCoords m'')
     putStrLn (drawCoords m''')
     putStrLn (drawCoords m'''')
     print (load m'''')
     --putStrLn (drawCoords (part2 inp))
     print (part2 inp)

-- part1 = {- lever right . lever below . lever left . -} lever above

lever dir m = rounds'
  where
    (cm,_) = M.findMin m
    (cM,_) = M.findMax m
    ground = [ (c,'.') | c <- range (cm,cM) ]
    cubes  = [ (c,'#') | (c,'#') <- M.assocs m ]
    m' = M.fromList cubes `M.union` M.fromList ground
    crs = [ c | (c,'O') <- M.assocs m ]
    rounds
      | dir == N = crs
      | dir == W = crs
      | dir == S = L.sortOn Down crs
      | dir == E = L.sortOn Down crs -- (\(C y x) -> (Down x,Down y)) crs
    rounds' = L.foldl' (f dir) m' rounds

f dir m c = M.insert c' 'O' m
  where
    move N = above; move W = left; move S = below; move E = right
    ds = takeWhile (inside m) (iterate (move dir) c)
    c' = last $ takeWhile (\d -> d == c || (m M.! d /= '#' && m M.! d /= 'O')) ds

inside m = inRange (cm,cM)
  where
    ((cm,_),(cM,_)) = (M.findMin m,M.findMax m)

load m = sum [ yM - y | (C y _,'O') <- M.assocs m ]
  where
    yM = 1 + maximum [ y | C _ y <- M.keys m ]

part2 m = load $ ms !! (first + i)
  where
    ms = iterate cyc m
    (first,next) = findRepeat ms
    i = (1_000_000_000 - first) `mod` (next - first)

cyc = lever E . lever S . lever W . lever N

findRepeat = go M.empty 0
  where
    go seen n (x:xs)
      | x `M.member` seen = (seen M.! x,n)
      | otherwise         = go (M.insert x n seen) (n+1) xs
