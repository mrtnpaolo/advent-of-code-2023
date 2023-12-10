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
     -- let (l,cs) = part2 inp
     -- putStrLn (showCoordsWith' 1 (f inp l cs) (A.assocs inp))

f a l cs c x
  | c `elem` l = "•" -- pure (a A.! c)
  | c `elem` cs = "I"
  | otherwise  = "."

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
    go 'S' _ _ = []
    go '.' _ _ = []
    go '|' c f | f == south = [(above c,south)] | f == north = [(below c,north)]
    go '-' c f | f == west  = [(right c,west )] | f == east  = [(left  c,east )]
    go 'L' c f | f == north = [(right c,west )] | f == east  = [(above c,south)]
    go 'J' c f | f == north = [(left  c,east )] | f == west  = [(above c,south)]
    go '7' c f | f == south = [(left  c,east )] | f == west  = [(below c,north)]
    go 'F' c f | f == south = [(right c,west )] | f == east  = [(below c,north)]

part2 a = (sum . map length $ rls, sum . map length $ rrs)
  where
    l = loop a
    (ls,rs) = mark (reverse l)
    boundary = S.fromList (map fst l)
    rls = regions a boundary ls
    rrs = regions a boundary rs

loop a = coords
  where
    entrance = head [ c | c <- range (A.bounds a), a A.! c == 'S' ]
    start = head (ways a entrance)
    coords = dfs (uncurry (next a)) start

{-
mark l@(S.fromList . map fst -> l') = go S.empty S.empty l
  where
    go ls rs [] = (ls `S.difference` l',rs `S.difference` l')
    go ls rs ((c,dir):cs)
      | dir == south = go (S.fromList [right c] `S.union` ls) (S.fromList [left  c] `S.union` rs) cs
      | dir == west  = go (S.fromList [below c] `S.union` ls) (S.fromList [above c] `S.union` rs) cs
      | dir == north = go (S.fromList [left  c] `S.union` ls) (S.fromList [right c] `S.union` rs) cs
      | dir == east  = go (S.fromList [above c] `S.union` ls) (S.fromList [below c] `S.union` rs) cs
-}

mark l@(S.fromList . map fst -> l') = go S.empty S.empty l
  where
    go ls rs [] = (ls `S.difference` l',rs `S.difference` l')
    go ls rs ((c,dir):cs)
      | dir == south
      = go (S.fromList [right c,below (right c)] `S.union` ls) (S.fromList [left  c,below (left  c)] `S.union` rs) cs
      | dir == west
      = go (S.fromList [below c,left  (below c)] `S.union` ls) (S.fromList [above c,left  (below c)] `S.union` rs) cs
      | dir == north
      = go (S.fromList [left  c,above (left  c)] `S.union` ls) (S.fromList [right c,above (right c)] `S.union` rs) cs
      | dir == east
      = go (S.fromList [above c,right (above c)] `S.union` ls) (S.fromList [below c,right (below c)] `S.union` rs) cs

regions a boundary = go S.empty
  where
    go seen cs
      | S.null cs = []
      | otherwise = region : go (seen `S.union` region) (cs `S.difference` region)
      where
        c = S.findMin cs
        region = S.fromList (bfs next c)
        next c = [ d | d <- neighbors c, area `inRange` d, d `S.notMember` boundary ]
        area = A.bounds a

{-

-- wrong 498 -- this is not considering all the neighbors of a turn in a loop

classify a boundary cs = go S.empty S.empty (cs `S.difference` boundary)
  where
    go inside outside candidates
      | S.null candidates = (inside,outside)
      | any ontheborder region = go inside (outside `S.union` region) (candidates `S.difference` region)
      | otherwise              = go (inside `S.union` region) outside (candidates `S.difference` region)
        where
          c = S.findMin candidates
          region = S.fromList (bfs next c)
          area = A.bounds a
          next c = [ d | d <- neighbors c, area `inRange` d, d `S.notMember` boundary ]
          ontheborder c = any (\d -> not (area `inRange` d)) (cardinal c)

pick a (S.fromList . map fst -> boundary) ls rs =
  case (length ols,length ors) of
    (0,_) -> ils
    (_,0) -> irs
  where
    (ils,ols) = classify a boundary ls
    (irs,ors) = classify a boundary rs

-}
{-

-- wrong 766 is too high -- this version doesn't take in consideration the jordan curve

part2 a = S.size $ classify a (S.fromList boundary) (S.fromList candidates) -- (length ins,ins)
  where
    start = head [ c | c <- range (A.bounds a), a A.! c == 'S' ]
    loop = takeWhile (\(d,_) -> a @@ d /= 'S') $ dfs (uncurry (next a)) (head (ways a start))
    ((b,_),(e,_)) = (head loop,last loop)
    boundary = start : (map fst loop)
    candidates = [ c | c <- range (A.bounds a), c `notElem` boundary ]
    --(outs,ins) = L.partition (infinite a boundary) candidates

classify a boundary = go S.empty S.empty
  where
    go inside outside candidates
      | S.null candidates = inside
      | any ontheborder region = go inside (outside `S.union` region) (candidates `S.difference` region)
      | otherwise              = go (inside `S.union` region) outside (candidates `S.difference` region)
        where
          c = S.findMin candidates
          region = S.fromList (bfs next c)
          area = A.bounds a
          next c = [ d | d <- neighbors c, area `inRange` d, d `S.notMember` boundary ]
          ontheborder c = any (\d -> not (area `inRange` d)) (neighbors c)

-}
