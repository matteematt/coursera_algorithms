
-- Heap.hs
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.IArray as IA
import Data.List
import Debug.Trace

data Heap = Heap {invariant :: Int -> Int -> Bool
                 ,bound :: Int
                 ,size :: Int
                 ,arr :: UArray Int Int}

instance Show Heap where
  show (Heap _ _ s a) = mconcat[show $ take s $ elems,", buffer size: ",show $ length elems]
    where elems = IA.elems a


initMinHeap :: Heap
initMinHeap = Heap (<) maxBound 0
              $ array (0,initBuffLen-1)
              $ zip [0..] $ take initBuffLen $ repeat maxBound

initMaxHeap :: Heap
initMaxHeap = Heap (>) minBound 0
              $ array (0,initBuffLen-1)
              $ zip [0..] $ take initBuffLen $ repeat minBound

hAdd :: Heap -> Int -> Heap
hAdd heap@(Heap i b s a) x =
  let heap' = Heap i b (s+1) $ runSTUArray $ do
      let (Heap _ _ _ a') = if fullBuffer heap then growBuffer heap else heap
      stArray <- thaw a'
      writeArray stArray s x
      return stArray
   in bubbleUp heap' s

hPeek :: Heap -> Maybe Int
hPeek (Heap _ _ b a) = if b == 0 then Nothing
                                 else Just $ head $ IA.elems $ a

hLen :: Heap -> Int
hLen (Heap _ _ b _) = b

hPop :: Heap -> (Heap, Maybe Int)
hPop heap@(Heap i b s a) =
  case s of 0 -> (heap, Nothing)
            _ -> let heap' = Heap i b (s-1) $ runSTUArray $ do
                      stArray <- thaw a
                      lastVal <- readArray stArray (s-1)
                      writeArray stArray (s-1) b
                      writeArray stArray 0 lastVal
                      return stArray
                  in (bubbleDown heap' 0, Just $ head $ IA.elems a)

-- Private methods
initBuffLen = 20 :: Int

swapVals :: Heap -> Int -> Int -> Heap
swapVals (Heap i b s a) l r = Heap i b s $ runSTUArray $ do
  stArray <- thaw a
  rVal <- readArray stArray r
  lVal <- readArray stArray l
  writeArray stArray r lVal
  writeArray stArray l rVal
  return stArray

growBuffer :: Heap -> Heap
growBuffer (Heap i b s a) =
  let
    oldElems = IA.elems a
    newElems = oldElems ++ (take (length oldElems) (cycle [b]))
   in Heap i b s $ array (0,(length newElems)-1) $ zip [0..] newElems

bubbleDown :: Heap -> Int -> Heap
bubbleDown heap@(Heap i b s a) index =
  let lci = 2 * index + 1
      rci = 2 * index + 2
      best = if lci <= s && i (a ! lci) (a ! index) then lci else index
      best' = if rci <= s && i (a ! rci) (a ! best) then rci else best
   in if best' == index
         then heap
         else let heap' = swapVals heap best' index
               in bubbleDown heap' best'

bubbleUp :: Heap -> Int -> Heap
bubbleUp heap@(Heap i b s a) index =
  let parentI = div index 2
   in if i (a ! parentI) (a ! index) || (a ! parentI) == (a ! index)
         then heap
         else let heap' = swapVals heap index parentI
               in bubbleUp heap' parentI

-- If the buffer is full we will need to create a larger array
fullBuffer :: Heap -> Bool
fullBuffer (Heap _ b s a) = s >= (lastI + 1)
  where lastI = snd $ bounds $ a

-- Main.hs
data MedianTracker = MedianTracker Heap Heap [Int]

run :: String -> String
run xs = let mainInts = map (read :: String -> Int) $ words xs
             minH = initMinHeap
             maxH = initMaxHeap
             (MedianTracker _ _ ms) = foldl' exec (MedianTracker minH maxH []) mainInts
             medians = trace (show $ reverse ms) (ms)
          in show $ sum medians `mod` 10000


exec :: MedianTracker -> Int -> MedianTracker
exec (MedianTracker minH maxH ms) x =
  let heapsWithNewVal = if Just x >= hPeek maxH then (hAdd minH x,maxH)
                                             else (minH,hAdd maxH x)
      -- (minH',maxH') = balanceHeaps heapsWithNewVal
      balancedHeaps = balanceHeaps heapsWithNewVal
      (minH',maxH') = trace (show balancedHeaps) (balancedHeaps)
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

testCase = "78\n\
  \71\n\
  \99\n\
  \9\n\
  \24\n\
  \11\n\
  \94\n\
  \96\n\
  \90\n\
  \14\n\
  \27\n\
  \60\n\
  \55\n\
  \46\n\
  \29\n\
  \74\n\
  \10\n\
  \67\n\
  \8\n\
  \97\n\
  \30\n\
  \18\n\
  \2\n\
  \43\n\
  \56\n\
  \98\n\
  \4\n\
  \33\n\
  \76\n\
  \86\n\
  \19\n\
  \41\n\
  \92"
