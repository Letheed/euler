module Pb114
  ( pb114
  ) where

import LibProblem

pb114 :: Computation
pb114 = answer' $ blockCombinations 50

blockCombinations :: Int -> Int
blockCombinations = (map countBlockCombinations [0..] !!)
  where countBlockCombinations nUnits
          | nUnits < 3 = 1
          | otherwise  = 1 + blockCombinations (nUnits-1) + (sum . map blockCombinations $ [0..nUnits-4])
