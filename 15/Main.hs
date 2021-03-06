module Main where

import qualified Data.Map as M
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

buildDistMap :: M.Map Int City -> M.Map (Int,Int) Double
buildDistMap cm =
  let n = length $ M.keys cm
      bm = M.fromList $ zip [(i,i)|i<-[1..n]] (repeat 0) :: M.Map (Int,Int) Double
      perms = [(x,y)|x<-[1..n],y<-[1..n],x/=y]
   in foldl' (\dm (x,y) -> M.insert (x,y) (getDist cm dm (x,y)) dm) bm perms

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

-- Run

-- cm = parseInput tc1
-- n = length $ M.keys cm
-- dm = buildDistMap cm
--
-- beforeEnd = exec n cm dm (M.fromList [(1,True)]) [(1,0.0)] 1
-- afterEnd = end beforeEnd dm

run :: String -> String
run input = let cm = parseInput input
                n = length $ M.keys cm
                dm = buildDistMap cm
                beforeLast = exec n cm dm (M.fromList [(1,True)]) [(1,0.0)] 1
                completed = end beforeLast dm
             in show $ floor $ sum $ map (\(_,d) -> d) $ completed

exec :: Int -> M.Map Int City -> M.Map (Int,Int) Double -> M.Map Int Bool -> [(Int,Double)] -> Int -> [(Int,Double)]
exec n cm dm visited ordered curr =
  let candidateIndexes = [(i)|i<-[1..n],M.member i visited == False]
      candidates = map (\i -> let (Just x) = M.lookup (curr,i) dm in (x,i)) candidateIndexes
      (dist,best) = head $ sortBy bestCity candidates
      visited' = M.insert best True visited
      ordered' = (best,dist) : ordered
   in if length ordered' == n
         then ordered'
         else trace ((show $ length ordered') ++ "/" ++ (show $ length $ M.keys cm))
                    (exec n cm dm visited' ordered' best)

end :: [(Int,Double)] -> M.Map (Int,Int) Double -> [(Int,Double)]
end ordering dm = let (latest,_) = head ordering
                      (Just dist) = M.lookup (latest,1) dm
                   in (1,dist) : ordering

bestCity :: (Double,Int) -> (Double,Int) -> Ordering
bestCity (d1,i1) (d2,i2) =
  if compare d1 d2 == EQ
     then compare i1 i2
     else compare d1 d2

-- Path : 1 3 2 5 6 4 1
-- TSP:15.2361 (15)
tc1 = "6\n\
  \1 2 1\n\
  \2 4 0\n\
  \3 2 0\n\
  \4 0 0\n\
  \5 4 3\n\
  \6 0 3"
