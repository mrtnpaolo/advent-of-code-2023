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
  do inp <- getInput parse 15
     print (part1 inp)
     print (part2 inp)
  where
    parse = L.splitOn "," . filter (/= '\n')

part1 = sum . map hash

hash = go 0
  where
    go n [] = n
    go n (x:xs) = go m xs
      where
        m = (17 * (n + ord x)) `mod` 256

f = sum . concat . map g . M.toAscList

g (box,lenses) = [ (box+1) * (i*n) | (i,(_,n)) <- zip [1..] lenses ]

part2 = f . M.filter (not . null) . go M.empty
  where
    go m [] = m
    go m ((L.splitOn "=" -> [x,read @Int -> n]):xs) =
      go (M.alter (insert lbl n) box m) xs
      where
        lbl = x
        box = hash x
    go m ((init -> x):xs) =
      go (M.adjust (remove lbl) box m) xs
      where
        lbl = x
        box = hash x

remove lbl [] = []
remove lbl ((x@(l,_)):xs)
  | l == lbl = xs
  | otherwise = x : remove lbl xs

insert :: String -> Int -> Maybe [(String,Int)] -> Maybe [(String,Int)]
insert lbl n Nothing = Just [(lbl,n)]
insert lbl n (Just xs)
  | Just m <- L.lookup lbl xs = Just (replace lbl n xs)
  | otherwise = Just (xs ++ [(lbl,n)])

replace lbl n [] = []
replace lbl n (x@(l,m):xs)
  | lbl == l  = (l,n) : xs
  | otherwise = x : replace lbl n xs
