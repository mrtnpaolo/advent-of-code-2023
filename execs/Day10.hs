{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
  do inp <- getInputArray 10
     print (part1 inp)
     print (part2 inp)

part1 a = (\n -> (n+1)`div`2) $ head $ map length $ loops
  where
    start = head [ c | c <- range (A.bounds a), a A.! c == 'S' ]
    -- bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
    -- dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
    -- path = dfsOn repr (uncurry (next a)) (ways a start)
    paths = map (dfsOn repr (uncurry (next a))) (ways a start)
    repr = id
    loops = map (takeWhile (\(d,_) -> a @@ d /= 'S')) paths

a @@ c = case a A.!? c of Just x -> x; Nothing -> '.'

ways a c =
  [ (d,south) | d <- [above c], a @@ d `elem` "|7F" ] ++
  [ (d,west ) | d <- [right c], a @@ d `elem` "-J7" ] ++
  [ (d,north) | d <- [below c], a @@ d `elem` "|LJ" ] ++
  [ (d,east ) | d <- [left  c], a @@ d `elem` "-LF" ]

next a c from = go (a @@ c) c from
  where
    go 'S' c _ = [ (d,f) | (d,f) <- zip (cardinal c) [south,west,north,east], a @@ d `elem` "|-LJ7F" ]
    go '|' c f | f == south = [(above c,south)] | f == north = [(below c,north)]
    go '-' c f | f == west  = [(right c,west )] | f == east  = [(left  c,east )]
    go 'L' c f | f == north = [(right c,west )] | f == east  = [(above c,south)]
    go 'J' c f | f == north = [(left  c,east )] | f == west  = [(above c,south)]
    go '7' c f | f == south = [(left  c,east )] | f == west  = [(below c,north)]
    go 'F' c f | f == south = [(right c,west )] | f == east  = [(below c,north)]
    go '.' _ _ = []
    go 'S' _ _ = []

part2 = const ()
