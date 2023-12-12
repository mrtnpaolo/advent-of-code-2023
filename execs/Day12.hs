module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.List.Split          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputLines parse 12
     print (part1 inp)
     print (part2 inp)
  where
    parse (words -> [xs,(map (read @Int) . L.splitOn "," -> ns)]) = (xs,ns)

part1 = sum . map f

f (xs,ns) = length [ ys | ys <- g xs, h ys == ns ]

g [] = [[]]
g ('?':xs) = map ('#':) ys ++ map ('.':) ys where ys = g xs
g (x:xs) = map (x:) (g xs)

h = map length . filter (not . null) . L.splitOn "."

part2 = const ()
