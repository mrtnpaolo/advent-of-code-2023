module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.Function
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputLines parse 7
     print (part1 c1 (parse1 inp))
     print (part1 c2 (parse2 inp))
  where
    parse (words -> [hand,read @Int -> bid]) = (hand,bid)
    parse1 = map \(hand,bid) -> (readH hand,bid)
    parse2 = map \(hand,bid) -> (readH' hand,bid)

readC c = let Just i = L.elemIndex c "23456789TJQKA" in i

data H
  = High | Pair | TwoPair | Three | Full | Four | Five
  deriving (Show,Enum,Eq,Ord)

readH orig@(L.group . L.sort -> xs) = (typ,orig)
  where
    typ = case L.sort (map length xs) of
      [5] -> Five
      [1,4] -> Four
      [2,3] -> Full
      [1,1,3] -> Three
      [1,2,2] -> TwoPair
      [1,1,1,2] -> Pair
      _ -> High

readH' xs
  | 'J' `notElem` xs = readH xs
  | otherwise = (typ,xs)
  where
    cs = "AKQT98765432"
    (js,nonjs) = L.partition ('J'==) xs
    candidates = sequence (replicate (length js) cs)
    (typ,_) = L.maximumBy (comparing fst) [ readH (replacements ++ nonjs) | replacements <- candidates ]

c1 = comparing fst <> comparing (map readC . snd)
c2 = comparing fst <> comparing (map (\case 'J' -> -1; c -> readC c) . snd)

part1 comp = sum . map (uncurry (*)) . zip [1..] . map snd . L.sortBy (comp `on` fst)
