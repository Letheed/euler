module Pb125
  ( pb125
  ) where

import LibProblem

import Batteries
import Data.List

uBound :: Int
uBound = 100000000

pb125 :: Computation
pb125 = answer' . sum $ consecutiveSquareSums

consecutiveSquareSums :: [Int]
consecutiveSquareSums = dedup . concatMap sumsWith . tails $ squares
  where squares = takeWhile (uBound `quot` 2 >) [n*n | n <- [1..]]
        sumsWith = filter isPalindromic . takeWhile (< uBound) . drop 2 . scanl' (+) 0
          where isPalindromic n = let digits = show n
                                  in digits == reverse digits
