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
  do inp <- getInputLines parse 9
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (read @Int) . words

part1 = sum . map f

diffs xs = zipWith (-) (tail xs) xs

f xs = sum (map last yss)
  where
    yss = takeWhile (not . all (0==)) . iterate diffs $ xs

part2 = const ()
