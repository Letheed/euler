module Pb20
  ( pb20
  ) where

import LibProblem

import Data.Char

pb20 :: Computation
pb20 = answer s $ show s ++ " Σ of the digits of 100!"
  where s = sum . map digitToInt . show . factorial $ (100::Integer)
