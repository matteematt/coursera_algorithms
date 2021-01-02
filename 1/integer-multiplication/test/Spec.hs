import Test.QuickCheck
import Lib

val1 = 3141592653589793238462643383279502884197169399375105820974944592
val2 = 2718281828459045235360287471352662497757247093699959574966967627

assert :: Bool -> String -> String -> String
assert x pass fail = if x then pass else fail

prop_simpleMulti x y = simple x y == x * y

-- Does not work as numbers are not the same length
prop_karatsubaMulti x y = karatsuba x y == x * y

main :: IO ()
main = do
  putStrLn $ assert (3 == 3) "3 is equal to 3" "3 is not equal to 3"
  quickCheck prop_simpleMulti
  quickCheck prop_karatsubaMulti
  putStrLn "Done with tests!"
