module Pb14
  ( pb14
  ) where

import LibProblem

import Data.Vector hiding ((++))

pb14 :: Computation
pb14 = answer n $ show n ++ " produces a Collatz sequence of length " ++ show (lenCollatzVec ! n)
  where n = maxIndex lenCollatzVec

lenCollatzVec :: Vector Int
lenCollatzVec = generate nMax lenCol
  where nMax = 1000000
        lenCol n
          | n < 2       = 1
          | next < nMax = 1 + lenCollatzVec ! next
          | otherwise   = 1 + lenCol next
          where next = collatz n
