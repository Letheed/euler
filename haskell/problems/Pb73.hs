module Pb73
  ( pb73
  ) where

import LibProblem

maxDepth :: Int
maxDepth = 12000

pb73 :: Computation
pb73 = answer' $ sizeSBTree (1,3) (1,2)

sizeSBTree :: (Int, Int) -> (Int, Int) -> Int
sizeSBTree (nl, dl) (nh, dh)
  | d > maxDepth = 0
  | otherwise = 1 + sizeSBTree (nl, dl) (n, d) + sizeSBTree (n, d) (nh, dh)
  where n = nl + nh
        d = dl + dh
