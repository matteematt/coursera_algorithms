module Main where

import Data.List

main :: IO ()
main = undefined

data Job = Job Int Int Int deriving Show

parseJobs :: String -> [Job]
parseJobs xs = map (\x -> let (w:l:_) = map (read) $ words x in Job w l (w-l))
                  $ tail $ lines xs

instance Eq Job where
  (==) (Job lw ll ls) (Job rw rl rs) =
    lw == rw && ll == rl && ls == rs

instance Ord Job where
  (<=) (Job lw ll ls) (Job rw rl rs) =
    if ls == rs then rw <= lw
                else ls <= rs
total = foldl' (\(acum,time) (Job w l _) -> let end = l+time in (acum + (w*end),end)) (0,0) $ sort $ parseJobs testData2

testData = "12\n\
  \8 50\n\
  \74 59\n\
  \31 73\n\
  \45 79\n\
  \24 10\n\
  \41 66\n\
  \93 43\n\
  \88 4\n\
  \28 30\n\
  \41 13\n\
  \4 70\n\
  \10 58"

testData2 = "2\n\
  \5 3\n\
  \2 1"
