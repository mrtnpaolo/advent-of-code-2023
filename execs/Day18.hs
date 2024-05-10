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
  do inp <- getInputLines parse 18
     let (_,s) = dig inp
     putStrLn (drawCoords (M.fromList [(c,'#')|c<-S.toList s]))
     print (C 0 0 `S.member` s)
     print (C 0 1 `S.member` s)
     print (C 1 0 `S.member` s)
     print (part1 s)
  where
    parse x = let [d:_,read @Int -> n,_] = f x in (d,n)
    f = words . map \case c | c `elem` "(#)" -> ' '; c -> c

dig = L.foldl' go (origin,S.singleton origin)
  where
    go (c,s) (d,n) = (c',s')
      where
        new = take n $ tail $ iterate (move d) c
        c' = last new
        s' = S.fromList new `S.union` s

move 'U' = above; move 'R' = right; move 'D' = below; move 'L' = left

part1 s = S.size s + length interior
  where
    interior = bfs inside (C 1 1)
    inside c = [ c' | c' <- neighbors c, c' `S.notMember` s ]
