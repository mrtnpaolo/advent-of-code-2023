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
import Control.Monad
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

    workflows (M.fromList . map workflow -> m) p = go "in"
      where
        go "A" = True
        go "R" = False
        go lbl = go ((m M.! lbl) p)

    workflow = (\[name,cut "," -> rules] -> (name,chain rules)) . words
      where
        chain :: [String] -> ([Int] -> String)
        chain rs p = let Left lbl = foldr1 (>=>) (map lift rs) p in lbl

        lift lbl@(cut ":" -> rule)
          | [(l:'<':(r -> n)),next] <- rule = \cases t | p l t < n -> Left next; t -> Right t
          | [(l:'>':(r -> n)),next] <- rule = \cases t | p l t > n -> Left next; t -> Right t
          | otherwise                       = \_ -> Left lbl

        p 'x' [x,_,_,_] = x; p 'm' [_,m,_,_] = m; p 'a' [_,_,a,_] = a; p 's' [_,_,_,s] = s

    part (L.splitWhen (`elem` " =,") -> ["","x",r->x,"m",r->m,"a",r->a,"s",r->s,""]) = [x,m,a,s]

part1 (flows,parts) = sum (sum <$> filter flows parts)

part2 = const ()
