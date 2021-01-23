module Main where

import qualified Data.Map as M

main :: IO ()
main = undefined

adjacencyList = map (map (read :: String -> Int) . words) $ lines $ testData
nodeList = M.fromList $ zip [1..((length adjacencyList) -1)] (repeat ([],[]))

addEdge :: M.Map Int ([Int],[Int]) -> (Int,Int) -> M.Map Int ([Int],[Int])
addEdge m (n,e) = undefined

mapUpdate :: Ord k => M.Map k a -> k -> (a -> a) -> M.Map k a
mapUpdate m k fn = let curr = M.lookup k m
                    in case curr of Nothing -> m
                                    Just (v) -> M.insert k (fn v) m

testData = "1 3\n\
  \2 1\n\
  \3 2\n\
  \4 2\n\
  \4 5\n\
  \5 6\n\
  \6 4\n\
  \7 6\n\
  \7 9\n\
  \8 7\n\
  \9 8"
