module Main where

import qualified Data.Map as M

main :: IO ()
main = undefined

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
        go _ 0 _ ss = ss
        go (w:ws) i m ss = if (M.lookup (i-1) m) >= ((+) <$> M.lookup (i-2) m <*> (Just w))
                              then go ws (i-1) m ss
                              else go (tail ws) (i-2) m ((w,i) : ss)

run :: String -> String
run input = let xs = parseInput input
                m = calcMax xs
             in show $ calcNodes xs m

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
