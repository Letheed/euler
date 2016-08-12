module Pb57
  ( pb57
  ) where

import LibProblem

import Control.Arrow
import Data.Bifunctor.Batteries
import Data.Ratio

pb57 :: Computation
pb57 = answer n $ show n ++ " fractions have a longer numerator than denominator"
  where n = length . filter hasLongerNumerator . take 1000 $ sqrt2Expansions

hasLongerNumerator :: Rational -> Bool
hasLongerNumerator = uncurry (>) . both (length . show) . (numerator &&& denominator)

sqrt2Expansions :: [Rational]
sqrt2Expansions = iterate nextExpansion (3%2)
  where nextExpansion r = n' % d'
          where d  = denominator r
                d' = numerator r + d
                n' = d' + d
