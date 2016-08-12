module Pb53
  ( pb53
  ) where

import LibProblem

pb53 :: Computation
pb53 = answer c $ show c ++ " (n choose k) > 10⁶, 1 ≤ n ≤ 100"
  where c = sum [if k * 2 == n then 1 else 2 | n <-[1..100::Integer]
                                             , k <-[1..n `quot` 2]
                                             , n `choose` k > 1000000]
