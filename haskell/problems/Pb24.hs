module Pb24
  ( pb24
  ) where

import LibProblem

import Data.List

pb24 :: Computation
pb24 = answer p $ show p ++ " millionth lexicographic permutation of 0..9"
  where (p, _) = foldr genPerm (0, (999999, [0..9])) (factorial <$> [0..9])
        genPerm f (p, (k, digits)) = (p * 10 + q, (k', digits'))
          where (i, k') = k `quotRem` f
                q = digits !! i
                digits' = delete q digits
