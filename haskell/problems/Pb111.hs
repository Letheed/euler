{-# LANGUAGE MultiWayIf #-}

module Pb111
  ( pb111
  ) where

import LibProblem(msgAnswer, answer', Computation)
import Data.List
import Data.Numbers.Primes

pb111 :: Computation
pb111 = answer' . sum $ (sum . genRepeatedDigitPrimes 10) <$> [0..9]

genRepeatedDigitPrimes :: Int -> Int -> [Int]
genRepeatedDigitPrimes size repDigit = head . dropWhile null $ genWithNonRepeatedDigits <$> [1..size-1]
  where nonRepDigits = delete repDigit [0..9]
        genWithNonRepeatedDigits nonrep
          | repDigit == 0 = nonRepPrimes
          | otherwise     = repPrimes ++ nonRepPrimes
          where repPrimes    = gen repDigit (size-1) nonrep
                nonRepPrimes = concatMap (\d -> gen d (size-1) (nonrep-1)) (delete 0 nonRepDigits)
        gen n digits nonrep
          | digits == 0      = [n | isPrime n]
          | nonrep == 0      = let n' = addRepDigit n digits in [n' | isPrime n']
          | digits == nonrep = nonRepPrimes
          | otherwise        = repPrimes ++ nonRepPrimes
          where n' d    = n * 10 + d
                digits' = digits - 1
                nonrep' = nonrep - 1
                repPrimes    = gen (n' repDigit) digits' nonrep
                nonRepPrimes = concatMap (\d -> gen (n' d) digits' nonrep') nonRepDigits
        addRepDigit n count
          | count == 0 = n
          | otherwise  = addRepDigit (n * 10 + repDigit) (count-1)
