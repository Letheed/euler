module Pb78
  ( pb78
  ) where

import LibProblem

import Data.Maybe
import qualified Data.Vector as V

uBound :: Int
uBound = 100000

pb78 :: Computation
pb78 = answer p $ show p
  where p = fromJust $ V.elemIndex 0 partitionsVec

partitionsVec :: V.Vector Int
partitionsVec = V.generate uBound partition
  where partition n
          | n == 0    = 1
          | otherwise = (`rem` 1000000) . sum . map (\(f, k) -> f k) . zip fs . takeWhile (<=n) $ pentagonals
          where fs = f : f : f' : f' : fs
                  where f  k =   partitionsVec V.! (n-k)
                        f' k = - partitionsVec V.! (n-k)

pentagonals :: (Integral a) => [a]
pentagonals = interweave pos neg
  where pos = scanl1 (+) [1, 4..]
        neg = scanl1 (+) [2, 5..]
        interweave (a:as) (b:bs) = a : b : interweave as bs
