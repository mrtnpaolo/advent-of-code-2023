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
  do inp <- getInputArray 16
     putStrLn (drawCoords $ M.fromList (A.assocs inp))
     let cs = concatMap (map fst) $ travel inp [(origin,E)]
     putStrLn (drawCoords $ M.fromList (zip cs (repeat '#')))
     print (part1 cs)
     let cs' = concatMap (map fst) $ travel inp [(C 0 3,S)]
     putStrLn (drawCoords $ M.fromList (zip cs' (repeat '#')))
     print (part2 inp)

part1 = length . S.fromList

travel a start = bfsOn repr nexts [start]
  where
    -- start = [(origin,E)]

    nexts = map n

    n (c,dir)
      | not (inside c) = []
      | otherwise      = filter (\(d,_) -> inside d) $
        case (a A.! c,dir) of

          ('.',_) -> [ (move dir c,dir) ]

          ('/',N) -> [ (right c,E) ]
          ('/',E) -> [ (above c,N) ]
          ('/',S) -> [ (left  c,W) ]
          ('/',W) -> [ (below c,S) ]

          ('\\',N) -> [ (left  c,W) ]
          ('\\',E) -> [ (below c,S) ]
          ('\\',S) -> [ (right c,E) ]
          ('\\',W) -> [ (above c,N) ]

          ('|',N) -> [ (above c,N) ]
          ('|',E) -> [ (above c,N), (below c,S) ]
          ('|',S) -> [ (below c,S) ]
          ('|',W) -> [ (above c,N), (below c,S) ]

          ('-',N) -> [ (left  c,W), (right c,E) ]
          ('-',E) -> [ (right c,E) ]
          ('-',S) -> [ (left  c,W), (right c,E) ]
          ('-',W) -> [ (left  c,W) ]

    repr = S.fromList

    b = A.bounds a
    inside = inRange b

move N = above; move E = right; move S = below; move W = left

part2 a = L.sortOn (Down . snd) es
  where
    (C ym xm,C yM xM) = A.bounds a
    edges =
      [ (C ym xm,E), (C ym xm,S) ] ++
      [ (C ym x,S) | x <- [xm+1..xM-1] ] ++
      [ (C ym xM,W), (C ym xM,S) ] ++
      [ (C y xM,W) | y <- [ym+1..yM-1] ] ++
      [ (C yM xM,N), (C yM xM,W) ] ++
      [ (C yM x,N) | x <- [xm+1..xM-1] ] ++
      [ (C yM xm,N), (C yM xm,E) ] ++
      [ (C y xm,E) | y <- [ym+1..yM-1] ]
    es = map (\(start,cs) -> (start,length . S.fromList . map fst . concat $ cs))
           [ (start,(travel a [start])) | start <- edges ]
