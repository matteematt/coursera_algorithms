module Lib where

import Data.List

simple :: Integer -> Integer -> Integer
simple x y = x * y

-- Assignment #1
-- Implement a multiplication algorithm that operates on single digit numbers as the base case
x = 3141592653589793238462643383279502884197169399375105820974944592
y = 2718281828459045235360287471352662497757247093699959574966967627

karatsuba :: Integer -> Integer -> Integer
karatsuba x y = if x <= 10 && y <= 10 then x * y else
                let
                    largestLen = if (length $ show x) > (length $ show y)
                                    then (length $ show x) else (length $ show y)
                    halfn = ceiling $ (fromIntegral largestLen) / 2
                    a = toInteger $ div x (10 ^ halfn)
                    b = toInteger $ mod x (10 ^ halfn)
                    c = toInteger $ div y (10 ^ halfn)
                    d = toInteger $ mod y (10 ^ halfn)
                    ac = karatsuba a c
                    bd = karatsuba b d
                    expanded = (a+b) * (c+d)
                    adbc = expanded - ac - bd
                in 10 ^ (2 * halfn) * ac + 10 ^ halfn * adbc + bd

-- Unused as can use (length $ show x) which appears as fast and less space required
base10digits :: Integer -> Integer
base10digits x = go x 1
    where go x i = if x > (10^i) then go x (i+1) else i
