module Main (main) where

import Advent          (getInputMap,left,right,frame)
import Data.Char       (isDigit)
import Data.Map.Strict (findWithDefault,fromListWith,assocs)

main =
  do m <- getInputMap 3
     let ps = parts m
     print (part1 ps)
     print (part2 ps)

parts m = fromListWith (++)
  [ (part,[read @Int (map at cs)])
  | (c,x) <- assocs m
  , not (isDigit . at . left $ c), isDigit x
  , let cs = takeWhile (isDigit . at) (iterate right c)
  , part <- take 1 [ (y,d) | d <- frame cs, let y = at d
                           , y /= '.', not (isDigit y) ] ]
  where
    at c = findWithDefault '.' c m

part1 ps = sum (sum <$> ps)

part2 ps = sum [ a*b | (('*',_),[a,b]) <- assocs ps ]
