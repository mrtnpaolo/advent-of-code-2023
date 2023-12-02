module Main (main) where

import Advent     (getInputLines)
import Data.Char  (isDigit, digitToInt)
import Data.Maybe (mapMaybe)
import Data.List  (tails, findIndex, isPrefixOf)

main =
  do inp <- getInputLines id 1
     print (part1 inp)
     print (part2 inp)

part1 = sum . map (value . map digitToInt . filter isDigit)

value xs = head xs * 10 + last xs

part2 = sum . map (value . mapMaybe decode . init . tails)

decode xs@(x:_)
  | isDigit x = Just  $  digitToInt x
  | otherwise = succ <$> findIndex (`isPrefixOf` xs) digits

digits = ["one","two","three","four","five","six","seven","eight","nine"]
