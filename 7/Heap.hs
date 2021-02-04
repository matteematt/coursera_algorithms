{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Heap(Heap, initMaxHeap, initMinHeap, initMinTestHeap, hAdd, hPeek, hPop, hLen) where

import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.IArray as IA

-- Was hoping to make a Heap generic in terms of values, but this does not
-- look possible https://stackoverflow.com/questions/2222997/stuarray-with-polymorphic-type

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
  let parentI = let pi = div (index-1) 2 in if pi < 0 then 0 else pi
   in if i (a ! parentI) (a ! index) || (a ! parentI) == (a ! index)
         then heap
         else let heap' = swapVals heap index parentI
               in if parentI == 0 then heap' else bubbleUp heap' parentI


-- If the buffer is full we will need to create a larger array
fullBuffer :: Heap -> Bool
fullBuffer (Heap _ b s a) = s >= (lastI + 1)
  where lastI = snd $ bounds $ a

-- TODO: Probably get rid of this and only use it for testing
initMinTestHeap :: [Int] -> Heap
initMinTestHeap xs = Heap (<) maxBound (length xs) (array (0,(length buffXs - 1)) $ zip [0..] buffXs)
  where buffXs = xs ++ (take ((length xs) * 4) $ repeat maxBound)
