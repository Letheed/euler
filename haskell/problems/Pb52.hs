module Pb52
  ( pb52
  ) where

import LibProblem

import Data.List

pb52 :: Computation
pb52 = answer n $ show n ++ " smallest int such that {2,3,4,5,6}x contain the same digits"
  where n = head [n | n <- [(1::Int)..], all (sameDigits n) ((n *) <$> [2..6])]
        sameDigits x y = null (show x \\ show y)
