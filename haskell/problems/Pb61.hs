module Pb61
  ( pb61
  ) where

import LibProblem

import Control.Arrow
import Data.List.Batteries

pb61 :: Computation
pb61 = answer s $ show s ++ " Σ of the set " ++ show set
  where s      = sum set
        set    = headThrow errMsg $ cyclicalFigurateSets 4 (8::Int)
        errMsg = "empty list (set not found)"

cyclicalFigurateSets :: (Integral a) => Int -> a -> [[a]]
cyclicalFigurateSets size order = concatMap (go tailSets . (:[])) headSet
  where (headSet:tailSets) = map (filterNumLen . polygonals) [order, order - 1..3]
          where filterNumLen = takeWhile (10^size >) . dropWhile (10^(size - 1) >)
        isCyclic x y = x `rem` mask == y `quot` mask
          where mask = 10^(size `quot` 2)
        go []    set
          | isCyclic' set = [set]
          | otherwise     = []
          where isCyclic' = uncurry isCyclic . (last &&& head)
        go polys set@(n:_) = concat [go rest (m:set) | (poly, rest) <- picks polys, m <- poly, isCyclic m n]
