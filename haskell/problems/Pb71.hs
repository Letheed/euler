module Pb71
  ( pb71
  ) where

import LibProblem

import Data.Ratio

pb71 :: Computation
pb71 = answer (numerator r) $ show r ++ " biggest fraction < 3/7 (with denominator ≤ 10⁶)"
  where r = maximum . filter (3 % 7 >) . map leftFraction $ [dMax - 10..dMax]
        leftFraction d = floor (fromIntegral d * 3 / 7) % d
        dMax = 1000000
