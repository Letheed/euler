module Pb46
  ( pb46
  ) where

import LibProblem

pb46 :: Computation
pb46 = answer n $ show n ++ " smallest odd composite disproving Goldbach's conjecture"
  where n = head . dropWhile followsGoldbach $ oddComposites

oddComposites :: [Int]
oddComposites = [n | n <- [9,11..], not (isPrime n)]

followsGoldbach :: Int -> Bool
followsGoldbach n = any (\p -> isSquare ((n - p) `quot` 2)) . takeWhile (n >) $ primes
  where isSquare x = x == (sqrtInt x)^(2::Int)
