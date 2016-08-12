module Pb27
  ( pb27
  ) where

import LibProblem

pb27 :: Computation
pb27 = answer (a*b) $ show (a, b) ++ " produce " ++ show n ++ " consecutive primes"
  where (n, a, b) = maximum [(consecutivePrimes a b, a, b) | a <- ps, b <- ps]
        ps = concatMap (\p -> [p, -p]) . reverse . takeWhile (1000 >=) $ primes

consecutivePrimes :: Int -> Int -> Int
consecutivePrimes a b = length . takeWhile (isPrime . abs . formula) $ [0..]
  where formula n = n*n + a*n + b
