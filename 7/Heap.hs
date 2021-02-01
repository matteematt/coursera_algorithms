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
  show (Heap _ _ s a) = mconcat[show $ take s $ elems,", buffer size: ",show $ length elems]
    where elems = IA.elems a

-- TODO: Probably get rid of this and only use it for testing
initMinTestHeap :: [Int] -> Heap
initMinTestHeap xs = Heap (<) maxBound (length xs) (array (0,(length buffXs - 1)) $ zip [0..] buffXs)
  where buffXs = xs ++ (take ((length xs) * 4) $ repeat maxBound)

initMinHeap :: Heap
initMinHeap = Heap (<=) maxBound 0
              $ array (0,initBuffLen-1)
              $ zip [0..] $ take initBuffLen $ repeat maxBound

hAdd :: Heap -> Int -> Heap
hAdd heap@(Heap i b s a) x =
  let heap' = Heap i b (s+1) $ runSTUArray $ do
      let (Heap _ _ _ a') = if fullBuffer heap then growBuffer heap else heap
      stArray <- thaw a'
      writeArray stArray s x
      return stArray
   in bubbleUp heap' s


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

bubbleUp :: Heap -> Int -> Heap
bubbleUp heap@(Heap i b s a) index =
  let parentI = div index 2
   in if i (a ! parentI) (a ! index)
         then heap
         else let heap' = swapVals heap index parentI
               in bubbleUp heap' parentI


-- If the buffer is full we will need to create a larger array
fullBuffer :: Heap -> Bool
fullBuffer (Heap _ b s a) = s >= (lastI + 1)
  where lastI = snd $ bounds $ a



-- Doesn't work because the type of stArray from thaw and stArray from newArray
-- don't match, so it doesn't work in the do statment
-- growBuffer :: Heap -> Heap
-- growBuffer (Heap i b s a) = Heap i b s $ runSTUArray $ do
  -- let oldLen = snd $ bounds $ a
  -- stArray <- thaw a
  -- stOldArray <- thaw a
  -- stNewArray <- newArray (0,(oldLen*2)-1) b
  -- forM_ [0..(oldLen-1)] $ \i -> do
    -- oldVal <- readArray stOldArray i
    -- writeArray stNewArray i oldVal
  -- return stNewArray
