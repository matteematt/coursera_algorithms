module Main where

import Debug.Trace
import Data.List
import qualified Data.Map as M

main :: IO ()
main = interact (show . run)

run :: String -> [Int]
run xs = take 5 $ reverse $ sort $ map (\(_,xs) -> length xs) $ M.toList $ kosaraju $ buildGraph xs

-- node => ([to],[from])
-- keeping to and from allows us to traverse the map backwards without copying it
type Graph = M.Map Int ([Int],[Int])

testGraph = buildGraph testCase3

kosaraju :: Graph -> M.Map Int [Int]
kosaraju graph = let visited = M.fromList $ zip [1..(length $ M.keys graph)] (repeat False)
                     ordering = kosarajuOrdering graph visited
                     leaderMap = M.fromList $ zip [1..(length $ M.keys graph)] (repeat [])
                     (_,lm) = foldl' (\(vis,lm) edge -> go2 graph vis edge lm edge) (visited,leaderMap) ordering
                  in lm


-- node => finish position order
visited :: M.Map Int Bool
visited = M.fromList $ zip [1..(length $ M.keys testGraph)] (repeat False)

-- Get order

-- Init map for finishing times and if visited
-- For each unvisited nodes (so we skipped checked nodes)
-- Mark node as visited, get neighbours (go backwards using FROM edges)
-- Recurse on neighbours
-- Mark this node as finished, with the ordering

kosarajuOrdering :: Graph -> M.Map Int Bool -> [Int]
kosarajuOrdering graph visisted =
  snd
  $ foldl' (\(vis,ord) edge -> go1 graph vis ord edge) (visited,[])
  $ reverse
  $ M.keys graph

-- Returns the ordering backwards
go1 :: Graph -> M.Map Int Bool -> [Int] -> Int -> (M.Map Int Bool,[Int])
go1 graph visited order curr =
  if M.lookup curr visited == Just True
     then (visited,order)
     else let visited' = M.insert curr True visited
              neighbours = bkwdNeighbours graph curr
              (vis,ord) = foldl' (\(vis,ord) next -> go1 graph vis ord next) (visited',order) neighbours
           in (vis,curr : ord)

-- Get SCC

-- Init map for node leaders and if visited
-- For each unvisited node (so we skip checked nodes)
-- Mark node as visited, set its leader in the leader map
-- get neighbours (go forwards using TO edges)
-- Recurse on neighbours

-- leader => [nodes]
leaderMap :: M.Map Int [Int]
leaderMap = M.fromList $ zip [1..(length $ M.keys testGraph)] (repeat [])

go2 :: Graph -> M.Map Int Bool -> Int -> M.Map Int [Int] -> Int -> (M.Map Int Bool,M.Map Int [Int])
go2 graph visited leader leaderMap curr =
  if M.lookup curr visited == Just True
     then (visited,leaderMap)
     else let visited' = M.insert curr True visited
              leaderMap' = mapValTransform leaderMap leader (\xs -> curr : xs)
              neighbours = fwdNeighbours graph curr
              (vis,lm) = foldl' (\(vis,lm) next -> go2 graph vis leader lm next) (visited',leaderMap') neighbours
           in (vis,lm)

groupedLeaders = snd
                 $ foldl' (\(vis,lm) edge -> go2 testGraph vis edge lm edge) (visited,leaderMap)
                 $ kosarajuOrdering testGraph visited

-- Graph query functions
fwdNeighbours :: Graph -> Int -> [Int]
fwdNeighbours graph k = _neighbours graph k fst

bkwdNeighbours :: Graph -> Int -> [Int]
bkwdNeighbours graph k = _neighbours graph k snd

_neighbours :: Graph -> Int -> (([Int],[Int]) -> [Int]) -> [Int]
_neighbours graph k fn = let v = M.lookup k graph
                          in maybe [] fn v

-- Build the graph from input
addEdge :: Graph -> (Int,Int) -> Graph
addEdge m (n,e) = let m' = mapValTransform m n (pushLHS e)
                   in mapValTransform m' e (pushRHS n)

mapValTransform :: Ord k => M.Map k a -> k -> (a -> a) -> M.Map k a
mapValTransform m k fn = let curr = M.lookup k m
                          in case curr of Nothing -> m
                                          Just v -> M.insert k (fn v) m

pushLHS :: Int -> ([Int],[Int]) -> ([Int],[Int])
pushLHS x (l,r) = (x:l,r)

pushRHS :: Int -> ([Int],[Int]) -> ([Int],[Int])
pushRHS x (l,r) = (l,x:r)

adjacencyList = map ((\(a:b:_) -> (a,b)) . map (read :: String -> Int) . words) $ lines testCase3


buildGraph x = let adjacencyList =
                     map ((\(a:b:_) -> (a,b)) . map (read :: String -> Int) . words) $ lines x
                   nodesCount = length $ group $ sort $ foldl' (\xs (a,b) -> a : b : xs) [] adjacencyList
                   nodeList = M.fromList $ zip [1..nodesCount] (repeat ([],[]))
                in foldl' addEdge nodeList adjacencyList

-- Confirmed working
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

-- Confirmed working
test2 = "1 2\n\
  \10 11\n\
  \10 7\n\
  \8 11\n\
  \11 9\n\
  \2 3\n\
  \2 4\n\
  \3 1\n\
  \3 8\n\
  \3 9\n\
  \4 5\n\
  \4 6\n\
  \5 7\n\
  \6 5\n\
  \7 6\n\
  \8 10\n\
  \8 6\n\
  \9 8"

-- Confirmed working
testCase1 = "1 4\n\
\2 8\n\
\3 6\n\
\4 7\n\
\5 2\n\
\6 9\n\
\7 1\n\
\8 5\n\
\8 6\n\
\9 7\n\
\9 3"

-- Confirmed working
testCase2 = "1 2\n\
  \2 6\n\
  \2 3\n\
  \2 4\n\
  \3 1\n\
  \3 4\n\
  \4 5\n\
  \5 4\n\
  \6 5\n\
  \6 7\n\
  \7 6\n\
  \7 8\n\
  \8 5\n\
  \8 7"

-- Not working
-- Doesn't build the correct graph
-- Fixed issue
testCase3 = "1 2\n\
\2 3\n\
\3 1\n\
\3 4\n\
\5 4\n\
\6 4\n\
\8 6\n\
\6 7\n\
\7 8"


