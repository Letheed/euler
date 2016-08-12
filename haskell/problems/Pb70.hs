module Pb70
  ( pb70
  ) where

import LibProblem hiding (primes, isPrime)

import Control.Arrow ((***), (&&&))
import Data.List
import Data.Numbers.Primes
import qualified Data.Set as S
import qualified Data.Vector as V

uBound :: Int
uBound = 10000000

pb70 :: Computation
pb70 = answer' . fst . minimumOn ratio . filter isPermutation . map (id &&& phi)  $ [2..uBound-1]
  where both f        = f *** f
        ratio         = uncurry (/) . both fromIntegral
        isPermutation = uncurry (==) . both (sort . show)

minimumOn :: (Ord b) => (a -> b) -> [a] -> a
minimumOn _ []     = error "minimum: empty list"
minimumOn f (x:xs) = fst $ foldl' minOn (x, f x) xs
  where minOn mn@(_,fMin) y
          | fy <= fMin = (y, fy)
          | otherwise  = mn
          where fy = f y

phi :: Int -> Int
phi n = round . (fromIntegral n *) . product . map ((\p -> 1 - 1 / p) . fromIntegral) $ factors n
  where factors = S.toList . (factorsVec V.!)

factorsVec :: V.Vector (S.Set Int)
factorsVec = V.generate uBound factors
  where factors n
          | isPrime n = S.singleton n
          | otherwise = S.insert fac facs
          where fac  = head [p | p <- primes, n `rem` p == 0]
                facs = factorsVec V.! (n `quot` fac)
