module Pb112
  ( pb112
  ) where

import LibProblem

pb112 :: Computation
pb112 = answer' $ limitNumber

limitNumber :: Int
limitNumber = go 100 101
  where go notBouncy n
          | isBouncy n = if notBouncy * 100 == n then n else go notBouncy (n+1)
          | otherwise  = go (notBouncy+1) (n+1)

isBouncy :: Int -> Bool
isBouncy n = go (q `quotRem` 10)
  where (q, d0) = n `quotRem` 10
        go (n', d1)
          | (d0 == d1) && n' /= 0 = go (n' `quotRem` 10)
          | otherwise             = breaksMonotony n' d1
          where ordering = compare d0 d1
                breaksMonotony m d0' = m /= 0 && (compare d1' d0' == ordering || breaksMonotony m' d1')
                  where (m', d1') = m `quotRem` 10

-- Performances measured with "perf stat --repeat 20"

-- order is reversed - 156.3 ms
-- digits n = if q == 0 then [r] else r : digits q
--   where (q, r) = n `quotRem` 10

-- order is reversed - 162.1 ms
-- digits 0 = [0]
-- digits n = unfoldr (\n -> if n == 0 then Nothing else Just $ swap (n `quotRem` 10)) n

-- order is conserved - 182.7 ms
-- digits n = go q0 [r0]
--   where (q0, r0) = n `quotRem` 10
--         go 0 ds = ds
--         go m ds = go q (r:ds)
--           where (q, r) = m `quotRem` 10
