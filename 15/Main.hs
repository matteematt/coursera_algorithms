module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Debug.Trace

main :: IO ()
main = interact run

data City = City Int Double Double deriving (Show, Eq)

-- Init
parseInput :: String -> M.Map Int City
parseInput input =
  M.fromList
  $ zip [1..]
  $ map ((\(i:x:y:_) -> City (read i) (read x) (read y)) . words)
  $ tail
  $ lines input


getDist :: M.Map Int City -> M.Map (Int,Int) Double -> (Int,Int) -> Double
getDist cm dm (x,y) =
  case M.lookup (y,x) dm
        of (Just v) -> v
           Nothing -> let (Just (City _ x1 y1)) = M.lookup x cm
                          (Just (City _ x2 y2)) = M.lookup y cm
                       in eDist (x1,y1) (x2,y2)

eDist :: (Double,Double) -> (Double,Double) -> Double
eDist (x1,y1) (x2,y2) = sqrt $ p2 (x2 - x1) + p2 (y2 - y1)
  where p2 x = x ^ 2

-- How many cities in each direction (index) to consider
-- smaller is faster but less accurate
searchWidth = 1000 :: Int

data TS = TS (M.Map Int City) (S.Set Int) [(Double,Int)] Int

run :: String -> String
run input =
  let cm = parseInput input
      n = length $ M.keys cm
      toVisit = S.fromList [1..n]
      ts = TS cm toVisit [(0.0,1)] 1
      (TS _ _ beforeLast _) = foldl' (\ts i -> trace (concat [show i,"/",show n]) exec ts) ts [2..n]
      afterLast = end cm beforeLast
   in show $ floor $ sum $ map (\(d,_) -> d) afterLast

exec :: TS -> TS
exec (TS cm toVisit visited curr) =
  let (Just (City _ cx cy)) = M.lookup curr cm
      index = S.findIndex curr toVisit
      toVisit' = S.deleteAt index toVisit
      lb = let x = index - searchWidth in if x < 0 then 0 else x
      ub = let x = index + searchWidth - lb in if x >= length toVisit' then (length toVisit') else x
      candidateIndexes = S.take ub $ S.drop lb toVisit'
      candidates = S.map (\i -> let (Just (City _ x y)) = M.lookup i cm in (eDist (x,y) (cx,cy),i)) candidateIndexes
      (dist,next) = S.findMin candidates
      visited' = (dist,next) : visited
   in toVisit' `seq` dist `seq` next `seq` TS cm toVisit' visited' next

end :: M.Map Int City -> [(Double,Int)] -> [(Double,Int)]
end cm visited =
  let (_,currI) = head visited
      (Just (City _ cx cy)) = M.lookup currI cm
      (Just (City _ lx ly)) = M.lookup 1 cm
      dist = eDist (cx,cy) (lx,ly)
   in (dist,1) : visited

-- Path : 1 3 2 5 6 4 1
-- TSP:15.2361 (15)
tc1 = "6\n\
  \1 2 1\n\
  \2 4 0\n\
  \3 2 0\n\
  \4 0 0\n\
  \5 4 3\n\
  \6 0 3"
