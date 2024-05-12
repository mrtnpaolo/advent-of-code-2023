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

r = read @Int
cut = L.splitOn

main =
  do inp <- getInput (parse . map clean) 19
     print (part1 inp)
     print (part2 inp)
  where
    clean c | c `elem` "{}" = ' ' | otherwise = c

    parse (cut "\n\n" -> [workflows . lines -> ws, map part . lines -> ps]) = (ws,ps)

    workflows (M.fromList . map (workflow . words) -> m) part = go "in"
      where
        go "A" = True
        go "R" = False
        go lbl = go ((m M.! lbl) part)

    workflow = (\[name,cut "," -> rules] -> (name,foldr check (error "not found") rules))
      where
        check lbl@(cut ":" -> rule) another part
          | [l:'<':(r -> n),next] <- rule, part `at` l < n = next
          | [l:'>':(r -> n),next] <- rule, part `at` l > n = next
          | ':' `notElem` lbl                              = lbl
          | otherwise                                      = another part

        [x,_,_,_] `at` 'x' = x
        [_,m,_,_] `at` 'm' = m
        [_,_,a,_] `at` 'a' = a
        [_,_,_,s] `at` 's' = s

    part (L.splitWhen (`elem` " =,") -> ["","x",r->x,"m",r->m,"a",r->a,"s",r->s,""]) = [x,m,a,s]

part1 (flows,parts) = sum (sum <$> filter flows parts)

part2 = const ()
