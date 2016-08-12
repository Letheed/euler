module Pb41
  ( pb41
  ) where

import LibProblem

import Batteries

pb41 :: Computation
pb41 = answer p $ show p ++ " largest n-digit pandigital prime"
  where p = head $ concatMap primePandigitals panLengths
        primePandigitals len = [pan | pan <- toNum <$> permutations' [len, len - 1..1], isPrime pan]
        -- filter out lengths for which pandigital numbers must be divisible by three
        --panLengths = filter (\n -> n `rem` 3 /= 0) [9,8..1]
        panLengths = [7, 4, 1]
