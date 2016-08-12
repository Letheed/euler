{-# LANGUAGE TupleSections #-}

module Pb66
  ( pb66
  ) where

import LibProblem

import Data.List
import Data.Ord

default (Integer, Double)

pb66 :: Computation
pb66 = answer d $ show d ++ " value of D ≤ 10³ in min solutions of x for which x is max"
  where (d, (x, _)) = maximumBy comparingX [(d, fundamentalSolution d) | d <- [2..1000], not (isSquare' d)]
          where comparingX = comparing (fst . snd)

fundamentalSolution :: Integer -> (Integer, Integer)
fundamentalSolution d = head . filter isSolution . continuants $ d
  where isSolution (x, y) = x*x - d * y*y == 1

continuants :: Integer -> [(Integer, Integer)]
continuants d = x1 : go x0 x1 (0, 1, n0)
  where n0 = sqrtInt d
        (x0, x1) = ((1, 0), (n0, 1))
        go (a0, b0) x1@(a1, b1) (m, e, n) = x2 : go x1 x2 params
          where params@(m', e', n') = (n * e - m, (d - m' * m') `quot` e, (n0 + m') `quot` e')
                x2 = (n' * a1 + a0, n' * b1 + b0)
