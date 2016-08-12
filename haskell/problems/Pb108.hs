module Pb108
  ( pb108
  ) where

import LibProblem(sqrtInt, msgAnswer, Computation)
import Data.Numbers.Primes

pb108 :: Computation
pb108 = msgAnswer $ show . head . filter ((1000 <) . nSolutions) $ [1..]

nSolutions :: Int -> Int
nSolutions n = (nDivisorsOfSquare n + 1) `quot` 2
  where nDivisorsOfSquare = product . map (\(_, e) -> 2 * e + 1) . primeFactorization

primeFactorization :: Int -> [(Int, Int)]
primeFactorization n = go n primes
  where sqrtN = sqrtInt n
        go 1 _        = []
        go m (p:ps)
          | p > sqrtN = [(m, 1)]
          | r == 0    = (p, e) : go q' ps
          | otherwise = go m  ps
          where (q, r) = m `quotRem` p
                (q', e) = reduce q 1
                reduce q e
                  | r' == 0   = reduce q' (e+1)
                  | otherwise = (q, e)
                  where (q', r') = q `quotRem` p
