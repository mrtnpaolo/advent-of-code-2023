module Main (main) where

import Advent             (getInputArray)
import Advent.Coord       (Coord(..),origin,east,south,turnLeft,turnRight,addCoord)
import Advent.Search      (AStep(..),astarN)
import Data.Char          (digitToInt)
import Data.Array.Unboxed (amap,bounds,(!))

main =
  do inp <- amap digitToInt <$> getInputArray 17
     let (_,cM) = bounds inp
     print (part1 cM inp)
     print (part2 cM inp)

data St = St !Coord !Coord !Int -- position, dir, distance
  deriving (Eq,Ord)

part1 = search 1 3
part2 = search 4 10

search distm distM dest@(C yd xd) a = best
  where
    best = head [ cost | (St c _ _,cost) <- astarN nexts starts, c == dest ]

    starts :: [St]
    starts = [ St origin east 0, St origin south 0 ]

    nexts :: St -> [AStep St]
    nexts (St c dir dist) =
      [ AStep { astepNext = St c' dir' dist'
              , astepCost = a ! c'
              , astepHeuristic = 0 }
      | (dir',dist') <-
        [ (dir,dist+1)      | dist <  distM ] ++
        [ (turnLeft  dir,1) | dist >= distm ] ++
        [ (turnRight dir,1) | dist >= distm ]
      , let c' = c `addCoord` dir'
      , inside c' ]

    inside (C y x) = 0 <= y && y <= yd && 0 <= x && x <= xd
