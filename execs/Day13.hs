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
     let reflections = map ref inp
     print (part1 reflections)
     print (part2 (zip reflections inp))
  where
    parse = L.splitOn "\n\n"

part1 = sum . map (\(r,c) -> r*100+c)

sym xss =
  [ i | i <- [1   ..n2 ], let (l,r) = L.splitAt i xss, reverse l `L.isPrefixOf` r ] ++
  [ i | i <- [n2+1..n-1], let (l,r) = L.splitAt i xss, r `L.isPrefixOf` reverse l ]
  where
    n = length xss; n2 = n `div` 2

ref (lines -> xss) = (fromMaybe 0 $ listToMaybe (sym xss), fromMaybe 0 $ listToMaybe (sym (L.transpose xss)))

part2 = part1 . map f

f ((r,c),xss) = head
  [ (fromMaybe 0 (listToMaybe r's),fromMaybe 0 (listToMaybe c's))
  | xs <- map lines $ smudges xss
  , let r's = filter (r /=) (sym xs)
  , let c's = filter (c /=) (sym (L.transpose xs))
  , not (null r's) || not (null c's)
  ]

smudges :: String -> [String]
smudges = init . go
  where
    go [] = [[]]
    go ('\n':xs) = map ('\n':) (go xs)
    go (c@'.':xs) = ['#':xs] ++ map (c:) (go xs)
    go (c@'#':xs) = ['.':xs] ++ map (c:) (go xs)
