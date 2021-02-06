module Main where

import Data.Array.Unboxed
import Data.List
import qualified Data.Map as M
import qualified Data.Array.IArray as IA
import Debug.Trace

main :: IO ()
main = interact run

inputToArr :: String -> UArray Int Int
inputToArr xs = let nums = map read $ words xs :: [Int]
                    processed = sort nums
                 in array (0,(length processed - 1)) (zip [0..] processed)

-- Move through each value, => x
-- Get a subarray which is
--  => -10000 < x+y < 10000
--     -10000-x < y < 10000-x
-- Mark in a map the values of x+y, don't could duplicate
-- answer is the number of values in the map

-- arr = inputToArr x

emptyMap :: M.Map Int Bool
emptyMap = M.fromList $ zip [] []

boundIndices :: UArray Int Int -> (Int,Int) -> (Int,Int)
boundIndices arr (lb,ub) = let (l,u) = bounds arr
                            in (lgo u lb l, rgo l ub u)
  where lgo b f i = if i >= b || arr ! (i) >= f then i else lgo b f (i+1)
        rgo b f i = if i <= b || arr ! (i) <= f then i else rgo b f (i-1)


run :: String -> String
run xs = let arr = inputToArr xs
             emptyMap = M.fromList $ zip [] []
             can = countAndUpdate arr
             validTs = foldl' (\m x -> can m x) emptyMap $ [0..((length $ IA.elems arr)-1)]
          in trace (show validTs) (show $ length $ M.keys validTs)
          -- in show $ length $ M.keys validTs


lowerB = negate 10000
upperB = 10000

-- lowerB = 0
-- upperB = 4

-- lowerB = 3
-- upperB = 10
-- arr = inputToArr testData
-- v = 20
-- (li,ui) = boundIndices arr (lowerB-v,upperB-v)
-- fromRange = map (\x -> arr ! x) [li..ui]
-- ts = map (\x -> arr ! x + v) [li..ui]

countAndUpdate :: UArray Int Int -> M.Map Int Bool -> Int -> M.Map Int Bool
countAndUpdate arr curr xi =
    let x = arr ! xi
        (li,ui) = boundIndices arr (lowerB-x,upperB-x)
        ts = map (\i -> trace (mconcat[show (arr ! i),"+",show x]) (arr ! i + x)) [yi|yi<-[li..ui],let y=(arr ! yi),yi/=xi, y >= lowerB, y <= upperB]
         -- ts = map (\y -> y + x) [y|yi<-[li..ui],let y=(arr ! yi),y/=x, y >= lowerB, y <= upperB]
      in foldl' (\m t -> M.insert t True m) curr ts


testData2 = "-2\n\
  \0\n\
  \0 \n\
  \4"

testData = "20\n\
  \6\n\
  \13\n\
  \7\n\
  \4\n\
  \2\n\
  \9\n\
  \9\n\
  \14\n\
  \18\n\
  \5\n\
  \13\n\
  \3\n\
  \19"

x = "-3\n\
    \-1\n\
    \1\n\
    \2\n\
    \9\n\
    \11\n\
    \7\n\
    \6\n\
    \2"
