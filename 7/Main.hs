module Main where

import Heap
import Data.List

main :: IO ()
main = interact run

data MedianTracker = MedianTracker Heap Heap [Int]

run :: String -> String
run xs = let mainInts = map (read :: String -> Int) $ words xs
             minH = initMinHeap
             maxH = initMaxHeap
             (MedianTracker _ _ medians) = foldl' exec (MedianTracker minH maxH []) mainInts
          in show $ sum medians `mod` 10000


exec :: MedianTracker -> Int -> MedianTracker
exec (MedianTracker minH maxH ms) x =
  let heapsWithNewVal = if Just x >= hPeek maxH then (hAdd minH x,maxH)
                                                else (minH,hAdd maxH x)
      (minH',maxH') = balanceHeaps heapsWithNewVal
      median = calcMedian minH' maxH'
   in MedianTracker minH' maxH' (median : ms)

calcMedian :: Heap -> Heap -> Int
calcMedian minH maxH =
  let maxLen = hLen maxH
      heapLen = maxLen + hLen minH
      medianI = if even heapLen then heapLen `div` 2 else (heapLen+1) `div` 2
      (Just m) = if medianI > maxLen then hPeek minH else hPeek maxH
   in m

balanceHeaps :: (Heap,Heap) -> (Heap,Heap)
balanceHeaps (minH,maxH) | sizeDiff >= 2 = let (minH',(Just x)) = hPop minH in (minH',hAdd maxH x)
                         | sizeDiff <= negate 2 = let (maxH',(Just x)) = hPop maxH in (hAdd minH x,maxH')
                         | otherwise = (minH,maxH)
  where minLen = hLen minH
        maxLen = hLen maxH
        sizeDiff = minLen - maxLen

