{-# LANGUAGE TupleSections #-}

module Pb85
  ( pb85
  ) where

import LibProblem

import Prelude hiding (max)

goal :: Int
goal = 2000000

pb85 :: Computation
pb85 = msgAnswer $ "grid: " ++ show solutionGrid ++ ", area = " ++ show (area solutionGrid)
  where area (a, b) = a * b

solutionGrid :: (Int, Int)
solutionGrid = go sol0 (nRect sol0, sol0)
  where sol0 = (1, max)
          where max = head . dropWhile ((< goal) . nRect . (1,)) $ [1..]
        go (m, n) (bestNRect, bestGrid)
          | m > n           = bestGrid
          | newNRect < goal = go (m+1, n) bestSolution
          | otherwise       = go (m, n-1) bestSolution
          where newNRect = nRect (m, n)
                bestSolution
                  | eps newNRect < eps bestNRect = (newNRect, (m, n))
                  | otherwise                    = (bestNRect, bestGrid)
                  where eps r = abs (r - goal)

{-# INLINE nRect #-}
nRect :: (Int, Int) -> Int
nRect (m, n) = m * n * (m+1) * (n+1) `quot` 4
