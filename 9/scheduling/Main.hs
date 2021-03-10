module Main where

import Data.List
import Debug.Trace

{-
number of jobs
weight length
weight length
-}

data DJob = DJob Integer Integer Integer deriving Show

instance Eq DJob where
  (==) (DJob lw ll ld) (DJob rw rl rd) = ld == rd

instance Ord DJob where
  (<=) (DJob lw ll ld) (DJob rw rl rd) =
    if rd == ld then lw <= rw
                else ld <= rd
  (compare) (DJob lw ll ld) (DJob rw rl rd) =
    if rd == ld then compare rw lw
                else compare rd ld

data RJob = RJob Integer Integer Double deriving Show

instance Eq RJob where
  (==) (RJob lw ll ld) (RJob rw rl rd) = ld == rd

instance Ord RJob where
  (<=) (RJob lw ll ld) (RJob rw rl rd) =
    if rd == ld then lw <= rw
                else ld <= rd

main :: IO ()
main = interact run

parseDJobs :: String -> [DJob]
parseDJobs xs = map ((\(w:l:_) -> DJob w l (w-l)) . (map read . words)) $ tail $ lines xs

convertDJob :: DJob -> RJob
convertDJob (DJob w l _) = RJob w l (fromInteger w / fromInteger l)

calcDJobs :: (Integer,Integer) -> DJob -> (Integer,Integer)
calcDJobs (time,acum) (DJob w l d) =
  let wt = w * (l + time)
   in (time+l,acum+wt)

calcRJobs :: (Integer,Integer) -> RJob -> (Integer,Integer)
calcRJobs (time,acum) (RJob w l d) =
  let wt = w * (l + time)
   in (time+l,acum+wt)

run :: String -> String
run xs = let djobs = parseDJobs xs
             rjobs = map convertDJob djobs
             (_,dt) = foldl' calcDJobs (0,0) $ sort djobs
             (_,rt) = foldl' calcRJobs (0,0) $ reverse $ sort rjobs
          in show dt ++ " " ++ show rt

