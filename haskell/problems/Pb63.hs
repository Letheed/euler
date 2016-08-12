module Pb63
  ( pb63
  ) where

import LibProblem

pb63 :: Computation
pb63 = answer s $ show s ++ " n-digit positive integers are also an nth power"
  where s = sum . takeWhile (0 /=) $ length . nthPowerOfSizeN <$> [1..]

nthPowerOfSizeN :: Int -> [Integer]
nthPowerOfSizeN n = takeWhile ((n ==) . numLen) . dropWhile ((n >) . numLen) . map (^n) $ [1..]
