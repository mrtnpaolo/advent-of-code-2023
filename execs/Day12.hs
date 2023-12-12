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

main =
  do inp <- getInputLines parse 12
     print (part1 inp)
     print (part2 inp)
  where
    parse (words -> [xs,(map (read @Int) . L.splitOn "," -> ns)]) = (xs,ns)

unfold = concat . L.intersperse "?" . replicate 5

part1 inp = sum (map f inp)

part2 = part1 . map (\(unfold -> xs,concat . replicate 5 -> ns) -> (xs,ns))

f (xs,ns) = go xs ns
  where
    go = memo2 \cases
      []         [] -> 1
      []         _  -> 0
      xs@('.':_) ns -> go (dropWhile ('.'==) xs) ns
      ('?':rest) ns -> go ('.':rest) ns + go ('#':rest) ns
      ('#':_   ) [] -> 0
      xs@('#':_) (n:ns)
        | length xs < n           -> 0
        | ('.'==) `any` take n xs -> 0
        | not (null ns) ->
          case drop n xs of
            []    -> 0
            '#':_ -> 0
            _     -> go (drop (n+1) xs) ns
        | otherwise -> go (drop n xs) ns

{-
f (unfold -> xs,concat . replicate 5 -> ns) = (xs,ys)
  where
    ys = L.splitOn "." xs
-}

{-
unfold c = concat . L.intersperse c . replicate 5

part2 = map f

f (unfold "?" -> xs,concat . replicate 5 -> ns) = count ((ns ==) . h) (g ns xs)

g [] _  = []
g _  [] = [[]]
g ns@(m:ms) (x:xs) =
  case x of
    '.' -> map ('.':) (g ns xs)
    '#' -> map ('#':) (g ((m-1):ms) xs)
    '?' -> map ('#':) (g ((m-1):ms) xs) ++ map ('.':) (g ns xs)

h = map length . filter (not . null) . L.splitOn "."
-}

{-
f (xs,ns) = length [ ys | ys <- g xs, h ys == ns ]

g [] = [[]]
g ('?':xs) = map ('#':) ys ++ map ('.':) ys where ys = g xs
g (x:xs) = map (x:) (g xs)

h = map length . filter (not . null) . L.splitOn "."

unfold (xs,ns) = (ys,ns)
  where
    ys = (concat . L.intersperse "?" . replicate 5) xs

part2 = sum . map (f . unfold)
-}

{-
main =
  do inp <- getInputLines parse 12
     print (part1 inp)
     print (part2 inp)
  where
    parse (words -> [xs,(map (read @Int) . L.splitOn "," -> ns)]) = (xs,ns)

part1 = sum . map f

f (xs,ns) = length [ ys | ys <- g xs, h ys == ns ]

g [] = [[]]
g ('?':xs) = map ('#':) ys ++ map ('.':) ys where ys = g xs
g (x:xs) = map (x:) (g xs)

h = map length . filter (not . null) . L.splitOn "."

part2 = const ()
-}
