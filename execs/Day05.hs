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
import Control.Applicative

main =
  do inp <- getInput parse 5
     print (part1 inp)
     print (part2 inp)
  where
    parse inp = (goals,maps)
      where
        (x:xs) = L.splitOn "\n\n" inp
        (words -> "seeds:":(map (read @Int) -> goals)) = x
        maps = map (map (map (read @Int) . words) . tail . lines) xs

part1 (goals,maps) = L.minimum $ map (\g -> L.foldl' dest g maps) goals

step n [dst,src,l]
  | src <= n && n <= src+l = Just $ dst + (n - src)
  | otherwise              = Nothing

dest n rs = fromJust $ asum [ step n r | r <- rs ] <|> Just n

f (a,b) [dst,src,l] -- ( matched, unmatched )

  -- a b     --    a b
  --     c d -- c d

  | b < c  || d < a  = ( [], [ (a,b) ] )

  --   a b   -- a b
  -- c     d -- c d

  | c <= a && b <= d = ( [ (dst+a-c,dst+b-c) ], [] )

  -- a   b   -- a   b
  --   c   d --   c d

  | a < c  && b <= d = ( [ (dst,dst+b-c) ], [ (a,c-1) ] )

  --   a   b -- a   b
  -- c   d   -- c d

  | c <= a && d < b  = ( [ (dst+a-c,dst+l-1) ], [ (d+1,b) ] )

  -- a     b
  --   c d

  | a < c  && d < b  = ( [ (dst,dst+l-1) ], [ (a,c-1), (d+1,b) ] )

  where
    c = src
    d = src+l-1

part2 (asRanges -> goals,maps) = minimum locs
  where
    fun (matched,unmatched) rule = (matched ++ m',u')
      where
        (concat -> m',concat -> u') = unzip $ map (\r -> f r rule) unmatched
    lvl rs = L.foldl' fun rs
    follow g = uncurry (++) . L.foldl' (\(m,u) -> lvl ([],m++u)) ([],g)
    locs = map fst $ concatMap (\g -> follow g maps) goals

asRanges [] = []
asRanges (start:width:rest) = [(start,start+width-1)] : asRanges rest
