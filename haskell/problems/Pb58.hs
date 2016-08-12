module Pb58
  ( pb58
  ) where

import LibProblem

pb58 :: Computation
pb58 = answer size $ show size ++ " side length for nPrime/nCorners < 10% " ++ show r
  where size = (nTot - 1) `quot` 2 + 1
        r@(_, nTot) = head . dropWhile ((0.1 <=) . ratio) $ ratios

ratio :: (Int, Int) -> Double
ratio (n, m) = fromIntegral n / fromIntegral m

ratios :: [(Int, Int)]
ratios = drop 1 $ scanl buildRatio (0, 1) [3, 5..]
  where buildRatio (nPrime, nTot) n = (nPrime', nTot + 4)
          where nPrime' = nPrime + (length . filter isPrime $ corners)
                corners = take 4 . iterate (subtract (n - 1)) $ (n * n)
