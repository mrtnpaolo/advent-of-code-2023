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
import Control.Applicative

main =
  do inp <- getInput parse 13
     print (part1 inp)
     print (part2 inp)
  where
    parse = map lines . L.splitOn "\n\n"

-- wrong 23566 28139
part1 xss = sum $ catMaybes [ ((100 *) <$> f xs) <|> f (L.transpose xs) | xs <- xss ]

f xs = listToMaybe
  [ i
  | let n = length xs
  , i <- [1..n-1]
  , let (a,b) = L.splitAt i xs
  , (i >= n `div` 2 && b `L.isPrefixOf` reverse a) ||
    (i <= n `div` 2 && (reverse a) `L.isPrefixOf` b)
  ]

part2 = const ()
