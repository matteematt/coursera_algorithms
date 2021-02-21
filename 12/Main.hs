module Main where

import qualified Data.Map as M
import Data.List

data Item = Item Int Int deriving Show

parseInput :: String -> (Int,[Item])
parseInput input = let (h:t) = lines input
                    in (read $ head $ words h,
                       map ((\(v:w:_) -> Item v w) . map read . words) t)

main :: IO ()
main = undefined

(knapsackWeight,items) = parseInput t1
empty = M.fromList $ zip [0..knapsackWeight] (repeat 0)

curr = head items

procItem :: Item -> Int -> M.Map Int Int -> M.Map Int Int -> Int -> M.Map Int Int
procItem (Item v w) weight prev next x =
  if

-- Best = 10
t1 = "7 5\n\
  \2 3\n\
  \2 1\n\
  \4 3\n\
  \5 4\n\
  \3 2"
