module Main where

import Data.List
import qualified Data.Map as M

main :: IO ()
main = undefined

testGraph = buildGraph testData


-- Build the graph from input

addEdge :: M.Map Int ([Int],[Int]) -> (Int,Int) -> M.Map Int ([Int],[Int])
addEdge m (n,e) = let m' = mapValTransform m n (pushLHS e)
                   in mapValTransform m' e (pushRHS n)

mapValTransform :: Ord k => M.Map k a -> k -> (a -> a) -> M.Map k a
mapValTransform m k fn = let curr = M.lookup k m
                          in case curr of Nothing -> m
                                          Just (v) -> M.insert k (fn v) m

pushLHS :: Int -> ([Int],[Int]) -> ([Int],[Int])
pushLHS x (l,r) = (x:l,r)

pushRHS :: Int -> ([Int],[Int]) -> ([Int],[Int])
pushRHS x (l,r) = (l,x:r)


buildGraph x = let adjacencyList =
                     map ((\(a:b:_) -> (a,b)) . map (read :: String -> Int) . words) $ lines $ x
                   nodesCount = length $ group $ sort $ map (\(x,_) -> x) adjacencyList
                   nodeList = M.fromList $ zip [1..nodesCount] (repeat ([],[]))
                in foldr (\nodeEdge m -> addEdge m nodeEdge) nodeList adjacencyList

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
