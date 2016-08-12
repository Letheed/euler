module Pb60
  ( pb60
  ) where

import LibProblem

import Data.List.Batteries

pb60 :: Computation
pb60 = answer s $ show s ++ " lowest Σ for a set of 5 primes " ++ show lighestSet
    where setSize = 5
          s = sum lighestSet
          lighestSet = headThrow errMsg (primePairSets setSize)
          errMsg     = "no prime pair set of size " ++ show setSize ++ " found (try with more primes)"

primePairSets :: Int -> [[Int]]
primePairSets size = go size (takeWhile (10000 >) primes) []
  where go 0 _      set = [reverse set]
        go _ []     _   = []
        go n (p:ps) set = go (n-1) (pairsOf p) (p:set) ++ go n ps set
          where pairsOf x = filter (concatsArePrimes x) ps
                concatsArePrimes x y = isPrime (concatNum x y) && isPrime (concatNum y x)
