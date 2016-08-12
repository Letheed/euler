module Pb76
  ( pb76
  , summations
  ) where

import LibProblem
import Data.List

pb76 :: Computation
pb76 = answer' . fst $ summations !! 99

summations :: [(Int, [Int])]
summations = (1, [1]) : (1, [1]) : go 2
  where go n = (sum sumCounts, sumCounts) : go (n+1)
          where summations' = take (n-2) . drop 1 $ summations
                sumCounts   = 1 : foldl' intermediateCount [1] (zip [n-1, n-2..] summations')
                intermediateCount newCounts (i, (sum', counts)) = newCount : newCounts
                  where newCount
                          | i > n `quot` 2 = sum' + 1
                          | otherwise      = sum . take i $ counts
