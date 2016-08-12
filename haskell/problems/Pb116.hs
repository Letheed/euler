module Pb116
  ( pb116
  ) where

import LibProblem

pb116 :: Computation
pb116 = answer' $ rgbBlockCombinations  50

rgbBlockCombinations :: Int -> Int
rgbBlockCombinations = (zipWith3 (\r g b -> r + g + b) redLst blueLst greenLst !!)
  where redLst   = blockCombinationsLst 2
        blueLst  = blockCombinationsLst 3
        greenLst = blockCombinationsLst 4

blockCombinationsLst :: Int -> [Int]
blockCombinationsLst blockLen = bCombsLst
  where bCombsLst = countBlockCombinations <$> [0..]
        countBlockCombinations nUnits
          | nUnits < blockLen = 0
          | otherwise         = 1 + bCombs (nUnits-1) + bCombs (nUnits-blockLen)
          where bCombs = (bCombsLst !!)
