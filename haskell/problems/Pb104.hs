module Pb104
  ( pb104
  ) where

import LibProblem
import Data.List

pb104 :: Computation
pb104 = answer' . fst3 . head . filter isDoubleEndedPandigital $ zip3 [0..] fibFront fibEnd
  where fst3 (a, _, _) = a
        fibEnd   = scanl (\x y -> (x + y) `rem` 1000000000) 0 (1:fibEnd)
        fibFront = 0 : 1 : go 0 1
          where go l m
                  | m > 10^(17::Int) = n : go (m `quot` 10) (n `quot` 10)
                  | otherwise = n : go m n
                  where n = l+m

isDoubleEndedPandigital :: (Int, Int, Int) -> Bool
isDoubleEndedPandigital (_, f, e) = endIsPand && frontIsPand
  where endIsPand   = [1..9] == (sort . digits $ e)
        frontIsPand = [1..9] == (sort . take 9 . digits' $ f)
        digits n = if q == 0 then [r] else r : digits q
          where (q, r) = n `quotRem` 10
        digits' n = go q0 [r0]
          where (q0, r0) = n `quotRem` 10
                go 0 ds = ds
                go m ds = go q (r:ds)
                  where (q, r) = m `quotRem` 10
