-- Works but was too slow for the problem
module Main where

import Data.List
import qualified Data.Map as M
import Debug.Trace

main :: IO ()
main = interact run

parsedInput :: [Int]
parsedInput = map (read :: String -> Int) $ words testData

hashMap :: M.Map Int Bool
hashMap = M.fromList $ zip parsedInput (repeat True)

numList :: [Int]
numList = M.keys hashMap

range :: [Int]
-- range = [0..10]
range = [negate 10000..10000]

emptyMap :: M.Map Int Bool
emptyMap = M.fromList $ zip [] []

run :: String -> String
run xs = let parsedInput = map (read :: String -> Int) $ words xs
             hashMap = M.fromList $ zip parsedInput (repeat True)
          in show $ sum $ map (\t -> trace (show t) (countVals hashMap parsedInput t)) range

countVals :: M.Map Int Bool -> [Int] -> Int -> Int
countVals m ks t = let (c,_) = foldl' go (0,emptyMap) ks in c
  where go (acum,seen) x = if M.lookup y m == Just True
                            && M.lookup y seen /= Just True
                            && y /= x
                            then (acum+1,M.insert x True $ M.insert y True seen)
                            else (acum,seen)
                              where y = t - x

testData = "20\n\
  \6\n\
  \13\n\
  \7\n\
  \4\n\
  \2\n\
  \9\n\
  \14\n\
  \18\n\
  \5\n\
  \3\n\
  \19"
