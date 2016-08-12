module Pb50
  ( pb50
  ) where

import LibProblem

import Data.List
import Data.Maybe

nMax:: Int
nMax = 1000000

pb50 :: Computation
pb50 = answer s $ show s ++ " Σ of " ++ show n ++ " consecutive primes"
  where (n, s) = maximum . mapMaybe primeSum . tails $ takeWhile (nMax >) primes

primeSum :: [Int] -> Maybe (Int, Int)
primeSum = listToMaybe . filter (isPrime . snd) . go 0 0 []
  where go _ _ sums []  = sums
        go n s sums (p:ps)
          | s' < nMax      = go n' s' (tns:sums) ps
          | otherwise      = sums
          where n' = n + 1
                s' = s + p
                tns = (n', s')
