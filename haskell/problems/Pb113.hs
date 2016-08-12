module Pb113
  ( pb113
  ) where

import LibProblem
import Data.Vector as V

pb113 :: Computation
pb113 = answer' $ nbCount 100

nbCount :: Int -> Int
nbCount = fst . (iterate nextOrder nbCountOrder1 !!)
  where nbCountOrder1 = (0, V.replicate 10 1)
        nextOrder (tot, vec) = (tot', vec')
          where vec' = generate 10 sumLower
                  where sumLower i
                          | i == 0    = 1
                          | otherwise = vec' ! (i-1) + vec ! i
                tot' = tot + vec' ! 9 + vec' ! 8 - 10
