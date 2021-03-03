module Main where

import qualified Data.Map as M
import Data.List
import Data.Bits
import Debug.Trace

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
main = interact run

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
-- n = length $ tail $ lines tc1

-- beforeEnd = exec (buildDistanceMatrix tc1) (buildBaseCase n)

buildBaseCase :: Int -> M.Map (Int,Int) InfNum
buildBaseCase n =
  foldl' (\m s -> M.insert (s,1) Inf m) emptys
  $ map (\x -> x `shiftL` 1) [1..(n-1)]
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

-- Main recurrence
-- exec :: Int -> DistMatrix -> M.Map (Int,Int) InfNum -> M.Map (Int,Int) InfNum
-- exec n c a =
  -- foldl' (\a m ->
    -- foldl' (\a si ->
      -- foldl' (\a j ->
        -- let klist = trace ("\n"++show a) (trace ("si=" ++ show si) (filter (\k -> k /= j) $ cIntToList n si))
            -- biiiiiiii = map (\k ->
              -- let (Just ckj) = M.lookup (j,k) c
                  -- withoutJ = trace ("ckj="++show ckj) (clearBit si (j-1))
                  -- mem = trace (concat["A[",show withoutJ,",",show k,"]=",
                        -- show $ memLookup a (withoutJ,k)])(memLookup a (withoutJ,k))
               -- in trace (concat["j=",show j," k=",show k," s-{j}=",show withoutJ]) (mem + (BNum ckj))
              -- ) klist
            -- best = trace (show biiiiiiii) (minimum biiiiiiii)
         -- in trace (concat["for ",show (si,j)," best=",show best]) (M.insert (si,j) best a)
        -- ) a $ filter (\x -> x /= 1) $ cIntToList n si
      -- ) a $ siCombinations m n
    -- ) a [2..n]

exec :: Int -> DistMatrix -> M.Map (Int,Int) InfNum -> M.Map (Int,Int) InfNum
exec n c a =
  foldl' (\a m ->
    trace ("m="++show m) (foldl' (\a si ->
      foldl' (\a j ->
        let klist = filter (\k -> k /= j) $ cIntToList n si
            biiiiiiii = map (\k ->
              let (Just ckj) = M.lookup (j,k) c
                  withoutJ = clearBit si (j-1)
                  mem = memLookup a (withoutJ,k)
               in mem + (BNum ckj)
              ) klist
            best = minimum biiiiiiii
         in M.insert (si,j) best a
        ) a $ filter (\x -> x /= 1) $ cIntToList n si
      ) a $ siCombinations m n)
    ) a [2..n]

-- Should miss return 0 or Inf?
memLookup :: M.Map (Int,Int) InfNum -> (Int,Int) -> InfNum
memLookup m sj =
  case M.lookup sj m of (Just v) -> v
                        Nothing -> Inf

end :: Int -> DistMatrix -> M.Map (Int,Int) InfNum -> InfNum
end n c a =
  minimum
  $ map (\j ->
    let (Just cj1) = M.lookup (j,1) c
        (Just asj) = M.lookup (fullset,j) a
     in asj + BNum cj1
    ) [2..n]
  where fullset = fullBitsSet n

fullBitsSet :: Int -> Int
fullBitsSet n = foldl' (\acum _ -> 1 .|. (acum `shiftL` 1)) 0 [1..n]

tc1 = "3\n\
  \0 0\n\
  \0 3\n\
  \3 3"

-- n = 4
-- beforeEnd = exec (buildDistanceMatrix tc1) (buildBaseCase n)
-- ans = end (buildDistanceMatrix tc1) beforeEnd

run :: String -> String
run input = let c = buildDistanceMatrix input
                n = length $ tail $ lines input
                beforeEnd = exec n c (buildBaseCase n)
             in show $ end n c beforeEnd

tc2 = "8\n\
\0 2.05\n\
\3.414213562373095 3.4642135623730947\n\
\0.5857864376269049 0.6357864376269047\n\
\0.5857864376269049 3.4642135623730947\n\
\2 0\n\
\4.05 2.05\n\
\2 4.10\n\
\3.414213562373095 0.6357864376269047"

tc3 = "4\n\
\0 0\n\
\4 3\n\
\4 0\n\
\0 3"

mainInput = "25\n\
\20833.3333 17100.0000\n\
\20900.0000 17066.6667\n\
\21300.0000 13016.6667\n\
\21600.0000 14150.0000\n\
\21600.0000 14966.6667\n\
\21600.0000 16500.0000\n\
\22183.3333 13133.3333\n\
\22583.3333 14300.0000\n\
\22683.3333 12716.6667\n\
\23616.6667 15866.6667\n\
\23700.0000 15933.3333\n\
\23883.3333 14533.3333\n\
\24166.6667 13250.0000\n\
\25149.1667 12365.8333\n\
\26133.3333 14500.0000\n\
\26150.0000 10550.0000\n\
\26283.3333 12766.6667\n\
\26433.3333 13433.3333\n\
\26550.0000 13850.0000\n\
\26733.3333 11683.3333\n\
\27026.1111 13051.9444\n\
\27096.1111 13415.8333\n\
\27153.6111 13203.3333\n\
\27166.6667 9833.3333\n\
\27233.3333 10450.0000"


-- n = 4
-- beforeEnd = exec tc2Matrix (buildBaseCase n)
-- ans = end tc2Matrix beforeEnd

-- tc2Matrix = M.fromList $
  -- zip [(x,y)|x<-[1..n],y<-[1..n]] [0.0,10.0,15.0,20.0,5.0,0.0,9.0,10.0,6.0,13.0,0.0,12.0,8.0,8.0,9.0,0.0] :: DistMatrix
