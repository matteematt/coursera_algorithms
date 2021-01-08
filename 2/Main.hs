module Main where

import Data.List

ans = "2407905288"

divideAndConquer :: Ord a => ([a] -> [a] -> [a]) -> [a] -> [a]
divideAndConquer _ [] = []
divideAndConquer _ [x] = [x]
divideAndConquer _ [x1,x2] = if x1 <= x2 then [x1,x2] else [x2,x1]
divideAndConquer fn xs = let (lhs,rhs) = splitAt (div (length xs) 2) xs
                             lhs' = divideAndConquer fn lhs
                             rhs' = divideAndConquer fn rhs
                          in fn lhs' rhs'

mergeSort :: Ord a => [a] -> [a] -> [a]
mergeSort [] [] = []
mergeSort (x:xs) [] = x : mergeSort xs []
mergeSort [] (y:ys) = y : mergeSort [] ys
mergeSort x@(x1:xs) y@(y1:ys) = if x1 <= y1 then x1 : mergeSort xs y
                                            else y1 : mergeSort x ys
-- Counting inversions
fastCI :: Ord a => ([a] -> Int -> [a] -> (Int,[a])) -> [a] -> (Int,[a])
fastCI _ [] = (0,[])
fastCI _ [x] = (0,[x])
fastCI _ [x1,x2] = if x1 <= x2 then (0,[x1,x2]) else (1,[x2,x1])
fastCI fn xs = let (lhs,rhs) = splitAt (div (length xs) 2) xs
                   (li,lhs') = fastCI fn lhs
                   (ri,rhs') = fastCI fn rhs
                   (si,xs') = fn lhs' (length lhs') rhs'
                in ((li + ri + si), xs')

fastSI :: Ord a => [a] -> Int -> [a] -> (Int,[a])
fastSI [] _ [] = (0,[])
fastSI (x:xs) _ [] = let (i,v) = fastSI xs 0 [] in (i, x : v)
fastSI [] _ (y:ys) = let (i,v) = fastSI [] 0 ys in (i, y : v)
fastSI x@(x1:xs) lenX y@(y1:ys) = if x1 <= y1 then let (i,v) = fastSI xs (lenX-1) y in (i, x1 : v)
                                              else let (i,v) = fastSI x lenX ys in (i+lenX, y1 : v)
inv :: [Int] -> Int
inv x = let (i,_) = fastCI fastSI x in i

run :: String -> String
run x = show $ inv $ map (read :: String -> Int) $ lines $ x

main :: IO ()
main = interact run
