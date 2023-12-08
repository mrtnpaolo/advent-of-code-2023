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
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInput parse 8
     print (part1 inp)
     print (part2 inp)
  where
    clean = map \case c | c `elem` "=(,)" -> ' ' | otherwise -> c
    f (words . clean -> [from,l,r]) = (from,(l,r))
    parse inp = (dirs,M.fromList g)
      where
        (dirs:_:(map f -> g)) = lines inp

move 'L' = fst
move 'R' = snd

part1 (dirs,g) = length $ go "AAA" (cycle dirs)
  where
    go "ZZZ" _ = []
    go node  (dir:nexts) = node : go node' nexts
      where
        node' = move dir (g M.! node)

part2 = const ()
