module Main where

import qualified Data.Map as M
import Data.List
import Data.Bits

type DistMatrix = M.Map (Int,Int) Double

-- Only works with +Inf as that is all we need for this problem
data InfNum = BNum Double | Inf deriving (Show, Ord, Eq)

instance Num InfNum where
  (+) (BNum x) (BNum y) = BNum $ x + y
  (+) x y               = Inf
  (*) (BNum x) (BNum y) = BNum $ x * y
  (*) x y               = Inf
  (abs) (BNum x)        = BNum $ abs x
  (abs) x               = x
  (signum) (BNum x)     = BNum $ signum x
  (signum) x            = BNum 1 -- only using +ve infinity
  (fromInteger) x       = BNum $ fromInteger x
  (-) (BNum x) (BNum y) = BNum $ x - y
  (-) Inf (BNum y)      = Inf
  (-) (BNum x) Inf      = undefined -- only using +ve infinity
  (-) Inf Inf           = undefined -- mathematically undefined

main :: IO ()
main = undefined

-- Setup, build matrix of distances C
parseCityCoords :: String -> [(Double,Double)]
parseCityCoords input = map ((\(x:y:_) -> (read x,read y)) . words) $ tail $ lines input

eDist :: (Double,Double) -> (Double,Double) -> Double
eDist (x1,y1) (x2,y2) = sqrt $ p2 (x2 - x1) + p2 (y2 - y1)
  where p2 x = x ^ 2

getDist :: M.Map Int (Double,Double) -> DistMatrix -> Int -> Int -> Double
getDist co cm c1 c2 =
  case M.lookup (c2,c1) cm
       of (Just dist) -> dist
          Nothing -> let (Just cc1) = M.lookup c1 co
                         (Just cc2) = M.lookup c2 co
                      in eDist cc1 cc2

buildDistanceMatrix :: String -> DistMatrix
buildDistanceMatrix input =
  let cityCoords = M.fromList $ zip [1..] (parseCityCoords input) :: M.Map Int (Double,Double)
      distances = M.fromList $ zip [(1,1)] [0] :: DistMatrix
      perms = [(x,y)|x<-[1..length cityCoords],y<-[1..length cityCoords]]
   in foldl' ((\dm (c1,c2) -> M.insert (c1,c2) (getDist cityCoords dm c1 c2) dm)) distances perms

-- Base case
-- TODO: don't hard code the value!
n = length $ tail $ lines tc1

buildBaseCase :: Int -> M.Map (Int,Int) InfNum
buildBaseCase n =
  foldl' (\m s -> M.insert (s,1) Inf m) emptys
  $ map (\x -> x `shiftL` 1) [1..n]
    where emptys = M.fromList $ zip [(1,1)] [(BNum 0)]

-- For m <- [2..n]
-- For each si <- set permuation of size m that contains 1 (represented as int)
-- For each j (city number) in si, where j /= 1
-- A[si,j] = minimum [ map (A[si - j,k] + C[kj]) k = (each city number in si where k /= j)
-- Finally, get the minimum distance to travel back to the start

-- converts a S represented as a Int into the constituent cities
cIntToList :: Int -> Int -> [Int]
cIntToList n s =
  concat $
  map (\i -> if s .&. (1 `shiftL` i) /= 0 then [i+1] else []) [0..n]

-- Creates a S of {1,2..n} for size m that contains 1
siCombinations :: Int -> Int -> [Int]
siCombinations r n = filter (\x -> x .&. 1 /= 0) $ combinations 0 0 r n []
    where combinations :: Int -> Int -> Int -> Int -> [Int] -> [Int]
          combinations set at r n subsets =
            if r == 0
              then set : subsets
              else concat $ map (\i ->
                let set' = set .|. (1 `shiftL` i)
                  in combinations set' (i+1) (r-1) n subsets
                          ) [at..(n-1)]


tc1 = "3\n\
  \0 0\n\
  \0 3\n\
  \3 3"
