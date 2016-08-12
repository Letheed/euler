module Pb87
  ( pb87
  ) where

import LibProblem(msgAnswer, answer', Computation)
import Data.List.Ordered
import Data.Numbers.Primes

uBound :: Int
uBound = 50000000

pb87 :: Computation
pb87 = do
  let primePowerTriples = [ppt | a <- fourths, b <- cubes, c <- squares, let ppt = a + b + c, ppt < uBound]
  answer' . length . nubSort $ primePowerTriples
  where squares = takeWhile (uBound >=) . map (\x -> x * x) $ primes
        cubes   = takeWhile (uBound >=) . zipWith (*) primes $ squares
        fourths = takeWhile (uBound >=) . map (\x -> x * x) $ squares
