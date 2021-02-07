-- Works but there are faster algorithm solutions without relying on multithreading
module Main where

import Data.List
import qualified Data.Map as M
import Debug.Trace
import Control.Parallel (par,pseq)

main :: IO ()
main = interact run

range :: [Int]
range = [negate 10000..10000]

emptyMap :: M.Map Int Bool
emptyMap = M.fromList $ zip [] []

run :: String -> String
run xs = let parsedInput = map (read :: String -> Int) $ words xs
             hashMap = M.fromList $ zip parsedInput (repeat True)
             pcalc r = map (\t -> trace (show t) (countVals hashMap parsedInput t)) r
             qlen = (div (length range) 4)
             r1 = pcalc (take qlen range)
             r2 = pcalc (take qlen $ drop qlen range)
             r3 = pcalc (take qlen $ drop (qlen*2) range)
             r4 = pcalc (take qlen $ drop (qlen*3) range)
             s1 = sum r1
             s2 = sum r2
             s3 = sum r3
             s4 = sum r4
             -- TODO: Can I use a fold to achieve this pattern?
          in show $ s1 `par` s2 `par` s3 `par` s4 `pseq` (s1+s2+s3+s4)

countVals :: M.Map Int Bool -> [Int] -> Int -> Int
countVals m ks t = foldl' go 0 ks
  where go acum x = if M.lookup y m == Just True
                            && y /= x
                            then 1
                            else acum
                              where y = t - x

testData = "20\n\
  \6\n\
  \13\n\
  \7\n\
  \4\n\
  \2\n\
  \9\n\
  \14\n\
  \18\n\
  \5\n\
  \3\n\
  \19"
