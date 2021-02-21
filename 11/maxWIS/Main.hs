module Main where

import qualified Data.Map as M

main :: IO ()
main = interact mainData

parseInput :: String -> [Int]
parseInput xs = map (read) $ tail $ lines $ xs

calcMax :: [Int] -> M.Map Int Int
calcMax xs = let m = M.fromList [(0,0),(1,head xs)]
              in go 2 (tail xs) m
  where go :: Int -> [Int] -> M.Map Int Int -> M.Map Int Int
        go i [] m = m
        go i (x:xs) m = let a' = M.lookup (i-1) m
                            a'' = (+) <$> M.lookup (i-2) m <*> (Just x)
                            (Just v) = max a' a''
                            m' = M.insert i v m
                         in go (i+1) xs m'

calcNodes :: [Int] -> M.Map Int Int -> [(Int,Int)]
calcNodes xs m = go (reverse xs) ((length $ M.keys m) - 1) m []
  where go :: [Int] -> Int -> M.Map Int Int -> [(Int,Int)] -> [(Int,Int)]
        go _ i _ ss      | i < 1 = ss
        go (w:ws) i m ss | otherwise = if (M.lookup i m) == M.lookup (i-1) m
                                          then go ws (i-1) m ss
                                          else go (tail ws) (i-2) m ((w,i) : ss)

checkList = [1,2,3,4,17,117,517,997] :: [Int]

run :: String -> String
run input = let xs = parseInput input
                m = calcMax xs
                nodes = calcNodes xs m
             in show $ filter (\(_,n) -> n `elem` checkList) nodes

tc1 = "10\n\
  \280\n\
  \618\n\
  \762\n\
  \908\n\
  \409\n\
  \34\n\
  \59\n\
  \277\n\
  \246\n\
  \779"

tc2 = "10\n\
  \460\n\
  \250\n\
  \730\n\
  \63\n\
  \379\n\
  \638\n\
  \122\n\
  \435\n\
  \705\n\
  \84"
