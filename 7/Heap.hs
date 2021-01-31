{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Heap where

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
  show (Heap _ _ s a) = show $ take s $ IA.elems a

initMinTestHeap :: [Int] -> Heap
initMinTestHeap xs = Heap (<) maxBound (length xs) (array (0,(length buffXs - 1)) $ zip [0..] buffXs)
  where buffXs = xs ++ (take ((length xs) * 4) $ repeat maxBound)

initMinHeap :: Heap
initMinHeap = Heap (<) maxBound 0
              $ array (0,initBuffLen-1)
              $ zip [0..] $ take initBuffLen $ repeat maxBound

hAdd :: Heap -> Int -> Heap
hAdd (Heap i b s a) x = Heap i b (s+1) $ runSTUArray $ do
  stArray <- thaw a
  writeArray stArray s x
  return stArray

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

-- If the buffer is full we will need to create a larger array
fullBuffer :: Heap -> Bool
fullBuffer (Heap _ b s a) = s >= (lastI + 1)
  where lastI = snd $ bounds $ a
