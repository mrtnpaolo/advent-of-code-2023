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
  do inp <- A.amap digitToInt <$> getInputArray 17
     let (cm,cM) = A.bounds inp
     print ("dest",cM)
{-
     let (a,b,xs) = part1 cM inp
     print a
     print b
     putStrLn $ showCoordsWith 1 pixel xs
-}
     mapM_ print (part1 cM inp)
     print (part2 inp)

{-
pixel N = "^"
pixel E = ">"
pixel S = "v"
pixel W = "<"

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c
-}

data St = St !Coord !Coord !Int -- position, dir, distance
  deriving (Eq,Ord,Show)

-- astarN ::  -> [(St,Int)
part1 dest@(C yd xd) a = [best]
  where
    best = head $ [ cost | (St c _ _,cost) <- astarN nexts starts, c == dest ]

    starts :: [St]
    starts = [ St origin east 0, St origin south 0 ]

    nexts :: St -> [AStep St]
    nexts (St c dir dist) =
      [ AStep { astepNext = St c' dir' dist'
              , astepCost = a A.! c'
              , astepHeuristic = 0 }
      | (dir',dist') <-
        [ (dir,dist+1) | dist < 3 ] ++
        [ (turnLeft  dir,1) ] ++
        [ (turnRight dir,1) ]
      , let c' = c `addCoord` dir'
      , inside c'
      ]

    inside (C y x) = 0 <= y && y <= yd && 0 <= x && x <= xd

{-
part1 dest@(C yd xd) a = take 10 ends
  where
    -- best = head $ L.sortOn (snd . fst3) $ take 100 ends

    ends = [ (c,cost) | (((c,_),_,_),cost) <- travel, c==C 1 5 ]

    -- astarOn :: Ord r => (a -> r) -> (a -> [(a,Int,Int)]) -> a -> [(a,Int)]
    travel = astarOn repr nexts start

    start = ( (origin,0), (S,2), [(origin,S)] )

    heat = (a A.!)

    repr = fst3

    nexts ((c,h),d,xs)
      | c == dest = []
      | otherwise = [ ( ((c',h + heat c'),d',xs++[(c',dir)])
                      , heat c'
                      , 0 )
                    | d'@(dir,_) <- new d, let c' = c `addCoord` fst d', inside c' ]

    inside (C y x) = 0 <= y && y <= yd && 0 <= x && x <= xd
-}

{-
-- too high: 1553 (1554 minus origin)
part1 dest@(C yd xd) a = best
  where
    best = head $ L.sortOn (snd . fst3) $ take 100 ends

    ends = filter ((dest ==) . fst . fst3) travel

    travel = bfsOn repr nexts starts

    starts =
      [ ( (right origin,heat (right origin)) , (E,1), [(right origin,E)] )
      , ( (below origin,heat (below origin)) , (S,1), [(below origin,S)] ) ]

    heat = (a A.!)

    repr = fst3

    nexts ((c,h),d,xs)
      | c == dest = []
      | otherwise = [ ((c',h + heat c'),d',xs++[(c',dir)])
                    | d'@(dir,_) <- new d, let c' = c `addCoord` fst d', inside c' ]

    inside (C y x) = 0 <= y && y <= yd && 0 <= x && x <= xd
-}

{-
new (E,3) = [(N,1),(S,1)]
new (E,n) = (E,n+1) : new (E,3)

new (W,3) = [(S,1),(N,1)]
new (W,n) = (W,n+1) : new (W,3)

new (N,3) = [(W,1),(E,1)]
new (N,n) = (N,n+1) : new (N,3)

new (S,3) = [(E,1),(W,1)]
new (S,n) = (S,n+1) : new (S,3)
-}

part2 = const ()
