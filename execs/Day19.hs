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

main =
  do inp <- getInput (parse . map clean) 19
     print (part1 inp)
     print (part2 inp)
  where
    clean c | c `elem` "{}" = ' ' | otherwise = c
    parse (L.splitOn "\n\n" -> [compose . map workflow . lines -> ws, map part . lines -> ps]) = (ws,ps)
    workflow = (\[name,L.splitOn "," -> rules] -> (name,f $ map xmas rules)) . words
    part (L.splitWhen (`elem` " =,") -> ["","x",r->x,"m",r->m,"a",r->a,"s",r->s,""]) = (x,m,a,s)

xmas xs
  | [cut "<" -> [l,r -> n],next] <- cut ":" xs = \t -> if p l t < n then Left next else Right t
  | [cut ">" -> [l,r -> n],next] <- cut ":" xs = \t -> if p l t > n then Left next else Right t
  where
    cut = L.splitOn
xmas xs  = const (Left xs)

f = ((\(Left xs) -> xs) .) . foldr1 (>=>)

compose ws t = go "in"
  where
    go xs = case (m M.! xs) t of "A" -> Just t; "R" -> Nothing; next -> go next
    m = M.fromList ws

p "x" = x
p "m" = m
p "a" = a
p "s" = s

x (x_,_,_,_) = x_
m (_,m_,_,_) = m_
a (_,_,a_,_) = a_
s (_,_,_,s_) = s_

part1 (ws,ps) = sum [ x+m+a+s | (x,m,a,s) <- mapMaybe ws ps ]

part2 = const ()
