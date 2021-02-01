module Main where

import Heap

import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.IArray as IA

main :: IO ()
main = undefined

testHeap = initMinTestHeap [0..4]
