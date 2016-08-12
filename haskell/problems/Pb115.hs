module Pb115
  ( pb115
  ) where

import LibProblem

default (Int)

bcLBound :: Int
bcLBound = 1000000

pb115 :: Computation
pb115 = msgAnswer $ show . head . dropWhile ((bcLBound >=) . snd) . zip [0..] . blockCombinationsLst $ 50

blockCombinationsLst :: Int -> [Int]
blockCombinationsLst minBlockLen = bCombsLst
  where bCombsLst = countBlockCombinations <$> [0..]
        countBlockCombinations nUnits
          | nUnits < minBlockLen = 1
          | otherwise            = 1 + bCombs (nUnits-1) + (sum . map bCombs $ [0..nUnits-(minBlockLen+1)])
          where bCombs = (bCombsLst !!)
