module Advent.Input
  ( getInput
  , getInputLines
  , getInputArray
  , getInputMap
  ) where

import Advent.Coord

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Array.Unboxed qualified as A
import Data.Map.Strict qualified as M

getInput :: (String -> a) -> Int -> IO a
getInput parse day =
  do args <- getArgs
     parse <$> case args of
       []    -> readFile (printf "inputs/input-%02d.txt" day)
       "-":_ -> getContents
       fn:_  -> readFile fn

getInputLines :: (String -> a) -> Int -> IO [a]
getInputLines parse day = getInput (map parse . lines) day

getInputArray :: Int -> IO (A.UArray Coord Char)
getInputArray day = makeArray <$> getInputLines id day
  where
    makeArray rows =
      A.listArray bounds (concat rows)
        where
          height = length rows
          width  = length (head rows)
          bounds = ( origin, C (height-1) (width-1) )

getInputMap :: Int -> IO (M.Map Coord Char)
getInputMap = getInput (M.fromList . withCoords id . lines)
