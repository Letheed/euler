module Pb48
  ( pb48
  ) where

import LibProblem

import Batteries

pb48 :: Computation
pb48 = answer n $ show n ++ " last ten digits of Σ [1 + 2^2 + 3^3 + ... + 1000¹⁰⁰⁰]"
  where n = readIntS . lastN 10 . show . sum $ [n^n | n <- [1..1000::Integer]]
