module Main (main) where

import Advent          (getInputLines,memo2)
import Data.List       (intercalate)
import Data.List.Split (splitOn)

main =
  do inp <- getInputLines parse 12
     print (part1 inp)
     print (part2 inp)
  where
    parse (words -> [xs,(map (read @Int) . splitOn "," -> ns)]) = (xs,ns)

part1 = sum . map (uncurry count)

part2 = part1 . map unfold
  where
    unfold (xs,ns) = (intercalate "?" (replicate 5 xs),concat (replicate 5 ns))

count = memo2 \cases
  []         [] -> 1
  []         _  -> 0
  xs@('.':_) ns -> count (dropWhile ('.'==) xs) ns
  ('?':rest) ns -> count ('.':rest) ns + count ('#':rest) ns
  ('#':_   ) [] -> 0
  xs@('#':_) (n:ns)
    | length xs < n           -> 0
    | ('.'==) `any` take n xs -> 0
    | not (null ns) ->
      case drop n xs of
        []    -> 0
        '#':_ -> 0
        _     -> count (drop (n+1) xs) ns
    | otherwise -> count (drop n xs) ns
