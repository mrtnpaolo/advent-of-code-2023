module Main (main) where

import Advent             (getInputArray)
import Advent.Search      (dfs,bfs)
import Advent.Coord       (south,west,north,east,below,left,above,right,neighbors)
import Data.Ix            (inRange)
import Data.Set           qualified as S
import Data.Array.Unboxed qualified as A

main =
  do a <- getInputArray 10
     let l = loop a
     print (part1 l)
     print (part2 l a)
     -- putStrLn $ f a

part1 = (`div` 2) . length

a @@ c = case a A.!? c of Just x -> x; Nothing -> '.'

-- discover the unique loop
loop a = dfs (next a) (begin a start)
  where
    [start] = [ c | (c,'S') <- A.assocs a ]

-- pick one of the (two) available pipes off the 'S'tart
begin a c = head $
  [ (d,south) | d <- [above c], a @@ d `elem` "|7F" ] ++
  [ (d,west ) | d <- [right c], a @@ d `elem` "-J7" ] ++
  [ (d,north) | d <- [below c], a @@ d `elem` "|LJ" ] ++
  [ (d,east ) | d <- [left  c], a @@ d `elem` "-LF" ]

-- move to the next pipe and associate the direction we're coming from
next a (c,from) = go (a @@ c) c from
  where
    go 'S' _ _ = []
    go '|' d f | f == south = [(above d,south)] | f == north = [(below d,north)]
    go '-' d f | f == west  = [(right d,west )] | f == east  = [(left  d,east )]
    go 'L' d f | f == north = [(right d,west )] | f == east  = [(above d,south)]
    go 'J' d f | f == north = [(left  d,east )] | f == west  = [(above d,south)]
    go '7' d f | f == south = [(left  d,east )] | f == west  = [(below d,north)]
    go 'F' d f | f == south = [(right d,west )] | f == east  = [(below d,north)]

-- assumption: the smallest union of same-side regions will be the inside one
part2 l a = sum (map length rls) `min` sum (map length rrs)
  where
    (ls,rs) = mark (reverse l)
    boundary = S.fromList (map fst l)
    rls = regions a boundary ls
    rrs = regions a boundary rs

-- follow the loop and mark non-loop cells on each side of the direction of travel
mark l@(S.fromList . map fst -> l') = go S.empty S.empty l
  where
    go ls rs [] = (ls `S.difference` l',rs `S.difference` l')
    go ls rs ((c,dir):cs)
      | dir == south = go (add ls [right,below.right]) (add rs [below,below.left ]) cs
      | dir == west  = go (add ls [below,left.below ]) (add rs [above,left.above ]) cs
      | dir == north = go (add ls [left ,above.left ]) (add rs [right,above.right]) cs
      | dir == east  = go (add ls [right,right.above]) (add rs [below,right.below]) cs
      where
        add xs fs = xs `S.union` S.fromList (sequence fs c)

-- expand a list of cells to a list of regions without duplicating work
regions a boundary = go S.empty
  where
    go seen cs
      | S.null cs = []
      | otherwise = region : go (seen `S.union` region) (cs `S.difference` region)
      where
        region = S.fromList (bfs nexts (S.findMin cs))
        nexts c = [ d | d <- neighbors c, area `inRange` d, d `S.notMember` boundary ]
        area = A.bounds a

{- -- debugging, had cells become part of multiple regions by accident
f a = showCoordsWith' 1 pixel (A.assocs a)
  where
    l = loop a
    cs = S.fromList (map fst l)
    (ls,rs) = mark (reverse l)
    rls = regions a cs ls
    rrs = regions a cs rs
    pixel c x
      | c `S.member` ls && c `S.member` rs = "°"
      | c `S.member` ls = "‹"
      | c `S.member` rs = "›"
      | any (c `S.member`) rls && any (c `S.member`) rrs = "x"
      | any (c `S.member`) rls = "«"
      | any (c `S.member`) rrs = "»"
      | c `S.member` cs = "•"
      | otherwise = pure '.'
-}
