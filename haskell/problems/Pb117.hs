module Pb117
  ( pb117
  ) where

import LibProblem
import Data.List

pb117 :: Computation
pb117 = do
  -- answer' $ blockCombinations 50
  answer' $ blockCombinationsLst !! 50

-- first version
blockCombinations :: Int -> Int
blockCombinations = bCombs
  where bCombs nUnits
          | nUnits < 0 = 0
          | otherwise  = bCombsLst !! nUnits
        countBlockCombinations nUnits
          | nUnits == 0 = 1
          | otherwise   = bCombs (nUnits-1) + bCombs (nUnits-2) + bCombs (nUnits-3) + bCombs (nUnits-4)
        bCombsLst = countBlockCombinations <$> [0..]

-- second version
blockCombinationsLst :: [Int]
blockCombinationsLst = 1 : 1 : 2 : 4 : zipWith4 (\a r g b -> a + r + g + b) aLst redLst greenLst blueLst
  where aLst     = drop 3 blockCombinationsLst
        redLst   = drop 2 blockCombinationsLst
        greenLst = drop 1 blockCombinationsLst
        blueLst  = blockCombinationsLst
