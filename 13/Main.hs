module Main where

import qualified Data.Map as M
import Data.List
-- import Data.List.Split
import Debug.Trace

type AdjMat = M.Map (Int,Int) InfInt

-- head tail length
data Edge = Edge Int Int Int deriving Show

-- prettyPrint :: AdjMat -> String
-- prettyPrint am =
  -- let xs = map (\(_,i) -> parse i) $ M.toList am :: [String]
   -- in unlines $ map unwords $ chunksOf 4  xs
  -- where parse i = case i of (BInt x) -> show x ++ " "
                            -- Inf      -> "Inf "

-- Only works with +Inf as that is all we need for this problem
data InfInt = BInt Int | Inf deriving (Show, Ord, Eq)

instance Num InfInt where
  (+) (BInt x) (BInt y) = BInt $ x + y
  (+) x y               = Inf
  (*) (BInt x) (BInt y) = BInt $ x * y
  (*) x y               = Inf
  (abs) (BInt x)        = BInt $ abs x
  (abs) x               = x
  (signum) (BInt x)     = BInt $ signum x
  (signum) x            = BInt 1 -- only using +ve infinity
  (fromInteger) x       = BInt $ fromInteger x
  (-) (BInt x) (BInt y) = BInt $ x - y
  (-) Inf (BInt y)      = Inf
  (-) (BInt x) Inf      = undefined -- only using +ve infinity
  (-) Inf Inf           = undefined -- mathematically undefined

-- run

main :: IO ()
main = interact (show . run)

(testAM,maxNode) = setup testInput
testDone = exec maxNode testAM

run :: String -> Maybe InfInt
run input =
  let (am,maxNode) = setup input
      am' = exec maxNode am
   in if checkNegCycle maxNode am'
         then Nothing
         else Just $ shortestPath maxNode am'

exec :: Int -> AdjMat -> AdjMat
exec maxNode am =
  foldl' (\amk k ->
    trace ("k=" ++ show k) (foldl' (\ami i ->
      foldl' (\amj j -> updateAM k i j amj) ami size)
    amk size))
  am size
  where size = [1..maxNode]

updateAM :: Int -> Int -> Int -> AdjMat -> AdjMat
updateAM k i j am = let viaK = getAMVal (i,k) am + getAMVal (k,j) am
                        withoutK = getAMVal (i,j) am
                     in if withoutK > viaK
                           then M.insert (i,j) viaK am
                           else M.insert (i,j) withoutK am


getAMVal :: (Int,Int) -> AdjMat -> InfInt
getAMVal ij am = case M.lookup ij am of (Just x) -> x
                                        Nothing -> Inf

checkNegCycle :: Int -> AdjMat -> Bool
checkNegCycle maxNode am =
  foldr (\i x -> if let (Just v) = M.lookup (i,i) am in v < (BInt 0) then True else x) False [1..maxNode]

-- Do I want to ignore the diagonals?
shortestPath :: Int -> AdjMat -> InfInt
shortestPath maxNode am =
  minimum
  $ map (\x -> let (Just i) = M.lookup x am in i)
  $ [(i,j)|i<-[1..maxNode],j<-[1..maxNode],i /= j]

-- Init Adjacency Matrix

setup :: String -> (AdjMat,Int)
setup input = let (am,maxNode) = readEdgeList $ parseEdgeList input
               in (foldl' (\amx i -> M.insert (i,i) 0 amx) am [1..maxNode], maxNode)

parseEdgeList :: String -> [Edge]
parseEdgeList xs =
  map ((\(t:h:l:_) -> Edge t h l) . map (read)  . words) $ tail $ lines xs

readEdgeList :: [Edge] -> (AdjMat, Int)
readEdgeList edgeList =
  foldl' (\(am,high) (Edge t h l) -> (M.insert (t,h) (BInt l) am, maximum [t,h,high]))
    (emptyAdjacecnyMatrix,0) edgeList
  where emptyAdjacecnyMatrix = M.fromList $ zip [(1,1)] [BInt 0] :: AdjMat

testInput = "Ignore\n\
  \1 3 -2\n\
  \3 4 2\n\
  \4 2 -1\n\
  \2 3 3\n\
  \2 1 4"

