module Pb94
  ( pb94
  ) where

import LibProblem

maxPerim :: Int
maxPerim = 1000000000

pb94 :: Computation
pb94 = answer' . sumPerims $ pTree
  where sumPerims (Node (a, b, c) c1 c2 c3)
          | perim > maxPerim                 = 0
          | base  == c + 1 || base  == c - 1 = perim  + restPerims
          | base' == c + 1 || base' == c - 1 = perim' + restPerims
          | otherwise                        = 0
          where base   = a * 2
                base'  = b * 2
                perim  = base  + 2 * c
                perim' = base' + 2 * c
                restPerims = sumPerims c1 + sumPerims c2 + sumPerims c3
