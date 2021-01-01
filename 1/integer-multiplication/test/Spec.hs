import Test.QuickCheck
import Lib

assert :: Bool -> String -> String -> String
assert x pass fail = if x then pass else fail

prop_simpleMulti x y = simple x y == x * y

main :: IO ()
main = do
  putStrLn $ assert (3 == 3) "3 is equal to 3" "3 is not equal to 3"
  quickCheck prop_simpleMulti
  putStrLn "Done with tests!"
