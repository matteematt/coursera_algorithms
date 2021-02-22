module Main where

import qualified Data.Map as M
import Data.List

data Item = Item Int Int deriving Show

main :: IO ()
main = interact run2

-- Part 1
-- This solution is too slow for part 2

parseInput :: String -> (Int,[Item])
parseInput input = let (h:t) = lines input
                    in (read $ head $ words h,
                       map ((\(v:w:_) -> Item v w) . map read . words) t)

run :: String -> String
run input = let (knapsackWeight,items) = parseInput input
                empty = M.fromList $ zip [0..knapsackWeight] (repeat 0)
                procItems' = procItems knapsackWeight empty
                finalMap = foldl' procItems' empty items
             in show $ M.lookup knapsackWeight finalMap

-- Iterate across each item
procItems :: Int -> M.Map Int Int -> M.Map Int Int -> Item -> M.Map Int Int
procItems maxWeight empty prev item = let procItem' = procItem item prev
                                       in foldl' procItem' empty [0..maxWeight]

-- Iterate across different weights in each item
procItem :: Item -> M.Map Int Int -> M.Map Int Int -> Int -> M.Map Int Int
procItem (Item v w) prev next x =
  let (Just prevScore) = M.lookup x prev
   in if w > x then M.insert x prevScore next
               else let (Just withItem) = (+) <$> M.lookup (x-w) prev <*> Just v
                     in M.insert x (max prevScore withItem) next

-- Part 2
-- Works on part 1 and part 2
-- Ran in 124.14 seconds
calcUsage :: M.Map Int Item -> M.Map (Int,Int) Int -> (Int,Int) -> (M.Map (Int,Int) Int,Int)
calcUsage items m (x,y) =
  let (Just (Item v w)) = M.lookup y items
      (m',without) = getMap m (x,y-1)
      (m'',with) = if w < x then getMap m' (x-w,y-1) else (m',negate v)
   in (m'', max without (with + v))
  where getMap m (x,y) =
          if y == 0 then (M.insert (x,0) 0 m,0)
                    else case
                      M.lookup (x,y) m
                        of (Just v) -> (m,v)
                           Nothing -> let (m',v) = calcUsage items m (x,y)
                                       in (M.insert (x,y) v m',v)

parseInputMap :: String -> (Int, M.Map Int Item)
parseInputMap input = let (kw,items) = parseInput input
                       in (kw, M.fromList $ zip [1..(kw+1)] items)

run2 :: String -> String
run2 input = let (kw,items) = parseInputMap input
                 blankMap = M.fromList $ zip [(0,0)] [0] :: M.Map (Int,Int) Int
                 (_,w) = calcUsage items blankMap (kw+1,length $ M.keys items)
              in show w

t1 = "7 5\n\
  \2 3\n\
  \2 1\n\
  \4 3\n\
  \5 4\n\
  \3 2"


t2 = "100 10\n\
  \9 46\n\
  \28 31\n\
  \15 42\n\
  \13 19\n\
  \31 48\n\
  \36 11\n\
  \13 27\n\
  \42 17\n\
  \28 19\n\
  \1 31"
