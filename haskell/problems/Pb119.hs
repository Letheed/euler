module Pb119
  ( pb119
  ) where

import LibProblem
import Data.List

pb119 :: Computation
pb119 = answer' . last . digitSumPowerLst $ 30

digitSumPowerLst :: Int -> [Integer]
digitSumPowerLst len = take len . sort . take (2*len) $ lst
  where lst = concat [dsp x | x <- [2..], 1 /= (sum . digits) x]
        dsp n = [m | (m, digitSum) <- takeWhile ((5*n >=) . snd) digitSumPowers, n == digitSum]
          where digitSumPowers = [(x, (sum . digits) x) | x <- iterate (*n) (n*n)]

digits :: Integer -> [Integer]
digits n = if q == 0 then [r] else r : digits q
  where (q, r) = n `quotRem` 10
