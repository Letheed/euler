module Pb21
  ( pb21
  ) where

import LibProblem

import qualified Data.Vector as V

pb21 :: Computation
pb21 = answer s $ show s ++ " Σ of all amicable numbers < 10⁴"
  where s = sum . concat $ [[n, m] | n <- [2..9999], let m = pfSum n, n < m, m < 10000, pfSum m == n]
        pfSum = (pfSumVec V.!)
          where pfSumVec = V.generate 10000 (sum . properFactors)
