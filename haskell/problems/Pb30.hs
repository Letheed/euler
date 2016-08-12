module Pb30
  ( pb30
  ) where

import LibProblem

pb30 :: Computation
pb30 = answer s $ show s ++ " Σ of numbers == to the Σ of 5th powers of their digits"
  where s = sum [n | n <- [2..6 * 9^5], n == sumPowDigits n]

sumPowDigits :: Int -> Int
sumPowDigits 0 = 0
sumPowDigits n = r^(5::Int) + sumPowDigits q
  where (q, r) = n `quotRem` 10
