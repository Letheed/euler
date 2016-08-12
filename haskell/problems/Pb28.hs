module Pb28
  ( pb28
  ) where

import LibProblem

size :: Int
size = 1001

pb28 :: Computation
pb28 = answer s $ show s ++ " Σ of the diagonals (" ++ show size ++ "x" ++ show size ++ " spiral)"
  where n = size `quot` 2
        s = 1 + 20 * (n * (n + 1) `quot` 2) + 4 * cornerSum
        cornerSum = sum . map corner $ [0..n-1]
          where corner i = 1 + 8 * i * (i + 1) `quot` 2
