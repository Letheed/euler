module Pb36
  ( pb36
  ) where

import LibProblem

import Data.Bits

pb36 :: Computation
pb36 = answer s $ show s ++ " Σ of all numbers < 10⁶ palindromic in bases 10 and 2"
  where s = sum [n | n <- [1..1000000], isPalindromicBase2 n, isPalindromic n]

isPalindromicBase2 :: Int -> Bool
isPalindromicBase2 n = n == mirror 0 n
  where mirror m 0 = m
        mirror m n = mirror m' n'
          where m' = shiftL m 1 + n .&. 1
                n' = shiftR n 1
