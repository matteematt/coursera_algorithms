module Main where

t1 = [3, 5, 20, 53] :: [Int]
t2 = [11, 15, 25, 40] :: [Int]

divideAndConquer :: Ord a => ([a] -> [a] -> [a]) -> [a] -> [a]
divideAndConquer _ [] = []
divideAndConquer _ [x] = [x]
divideAndConquer _ [x1,x2] = if x1 <= x2 then [x1,x2] else [x2,x1]
divideAndConquer fn xs = let (lhs,rhs) = splitAt (div (length xs) 2) xs
                             lhs' = divideAndConquer mergeSort lhs
                             rhs' = divideAndConquer mergeSort rhs
                          in fn lhs' rhs'

mergeSort :: Ord a => [a] -> [a] -> [a]
mergeSort [] [] = []
mergeSort (x:xs) [] = x : mergeSort xs []
mergeSort [] (y:ys) = y : mergeSort [] ys
mergeSort x@(x1:xs) y@(y1:ys) = if x1 <= y1 then x1 : mergeSort xs y
                                            else y1 : mergeSort x ys


splitInversions :: Ord a => [a] -> [a] -> Int -> [a]
splitInversions [] [] i = (i,[])
splitInversions (x:xs) [] i = let (i',v) = splitInversions xs [] i in x :
splitInversions [] (y:ys) i = y : splitInversions [] ys (i+1)
splitInversions x@(x1:xs) y@(y1:ys) i = if x1 <= y1 then (_,x1 : splitInversions xs y i)
                                                    else (_,y1 : splitInversions x ys (i+1))

main :: IO ()
main = undefined
