module Pb16
  ( pb16
  ) where

import LibProblem

import Data.Char
import Data.Bits

pb16 :: Computation
pb16 = answer s $ show s ++ " Σ of the digits of 2¹⁰⁰⁰"
  where s = sum . map digitToInt . show $ (1::Integer) `shiftL` 1000
