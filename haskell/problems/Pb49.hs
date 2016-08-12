module Pb49
  ( pb49
  ) where

import LibProblem

import Batteries
import Data.List

pb49 :: Computation
pb49 = answer n $ show seq ++ " 2nd 4-digit sequence of permutative primes"
  where n = t1 * 10000^2 + t2 * 10000 + t3
        seq@(t1, t2, t3) = tripletSequences !! 1

tripletSequences :: [(Int, Int, Int)]
tripletSequences = concatMap sequences candidateLists
  where primes4Digits = filter isPrime [1000..9999]
        candidateLists = filter (lengthGT 2) . sortGroupOn digits $ primes4Digits
        digits = sort . show
        triplets = map (\[t1, t2, t3] -> (t1, t2, t3)) . combinationsOf 3
        sequences = filter (\(t1, t2, t3) -> t2 - t1 == t3 - t2) . triplets
