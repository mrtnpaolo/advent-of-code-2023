module Main (main) where

import Advent     (getInputLines)
import Data.List  (sortOn,sort,group,elemIndex,nub)
import Data.Maybe (mapMaybe)

main =
  do inp <- getInputLines parse 7
     print (part1 inp)
     print (part2 inp)
  where
    parse (words -> [hand,read @Int -> bid]) = (hand,bid)

winnings hands = sum [ rank*bid | rank <- [1..] | (_,bid) <- hands ]

part1 = winnings . sortOn (\(hand,_) -> classify hand : value "23456789TJQKA" hand)

classify (sort . map length . group . sort -> typ) = n
  where
    Just n = typ `elemIndex` [[1,1,1,1,1],[1,1,1,2],[1,2,2],[1,1,3],[2,3],[1,4],[5]]

value cards = mapMaybe (`elemIndex` cards)

part2 = winnings . sortOn (\(hand,_) -> jokers hand : value "J23456789TQKA" hand)

jokers hand = maximum [ classify (map replace hand)
                      | card <- nub hand
                      , let replace = \case 'J' -> card; x -> x ]
