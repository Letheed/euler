module Pb39
  ( pb39
  , perimeters
  ) where

import LibProblem

import Control.Arrow
import Data.Ord
import Data.List

pb39 :: Computation
pb39 = answer p $ show p ++ " perimeter with most (" ++ show len ++ ") right triangle solutions < 10³"
  where (len, p) = maximumBy (comparing fst) . map (length &&& head) . group . sort $ perimeters

perimeters :: [Int]
perimeters = go pTree
  where go (Node (a, b, c) c1 c2 c3)
          | perim > 1000 = []
          | otherwise    = perims ++ go c1 ++ go c2 ++ go c3
          where perim  = a + b + c
                perims = perim : (takeWhile (1000 >=) . map (perim *) $ [2..])
