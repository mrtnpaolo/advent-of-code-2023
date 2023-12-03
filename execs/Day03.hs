{-# OPTIONS_GHC -Wno-unused-local-binds #-}
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
  do inp <- getInput (parse . lines) 3
     print (part1 inp)
     print (part2 inp)
  where
    parse = M.fromList . withCoords id

part1 m = sum [ n | (n,cs) <- collect m, isPart m cs ]

collect m = ns
  where
    Just (cm@(C ym xm),cM@(C yM xM)) = boundingBox (M.keys m)
    go c@(C y x) ds cs
      | y > yM = []
      | x > xM = (ds,cs) : go (below (C y 0)) [] []
      | isDigit (m M.! c) = go (right c) (ds++[m M.! c]) (cs++[c])
      | otherwise = (ds,cs) : go (right c) [] []
    ns = map (\(a,b) -> (read @Int a,b)) $ filter (not . null . fst) $ go origin [] []

isPart m cs = any isSymbol' [ m M.! c | c@(C y x) <- concatMap neighbors cs, inside c ]
  where
    Just (cm@(C ym xm),cM@(C yM xM)) = boundingBox (M.keys m)
    inside (C y x) = ym <= y && y <= yM && xm <= x && x <= xM

isSymbol' x = not (isDigit x) && not (x == '.')

part2 m = sum gs
  where
    ns = collect m
    gs = [ product $ map fst ns' | (c,'*') <- M.assocs m
             , let ns' = filter (\(n,cs) -> any (`elem` neighbors c) cs) ns
             , length ns' == 2 ]
