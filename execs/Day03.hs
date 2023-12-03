module Main (main) where

import Advent          (getInputMap,left,right,neighbors)
import Data.Char       (isDigit)
import Data.Maybe      (maybeToList)
import Data.List       (find)
import Data.Map.Strict (findWithDefault,fromListWith,assocs)

main =
  do m <- getInputMap 3
     let ps = parts m
     print (part1 ps)
     print (part2 ps m)

m @@ c = findWithDefault '.' c m

parts m = fromListWith (++)
  [ (part,[n])
  | (c,x) <- assocs m
  , isDigit x, not (isDigit (m @@ left c))
  , let cs = takeWhile (isDigit . (m @@)) (iterate right c)
  , part <- maybeToList (find (isSymbol . (m @@)) (concatMap neighbors cs))
  , let n = read @Int (map (m @@) cs)
  ]

isSymbol x = x /= '.' && not (isDigit x)

part1 ps = sum (sum <$> ps)

part2 ps m = sum [a*b | ((m @@) -> '*',[a,b]) <- assocs ps]
