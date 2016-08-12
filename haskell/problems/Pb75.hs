module Pb75
  ( pb75
  ) where

import LibProblem
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

uBound :: Int
uBound = 1500000

pb75 :: Computation
pb75 = answer' . V.length . V.filter (== 1) $ perimVec

perimVec :: V.Vector Int
perimVec = runST $ do
  vec <- MV.replicate (uBound+1) 0
  let ms = [2..sqrtInt (uBound `quot` 2)]
  forM_ ms $ \m -> do
    let ns
          | even m    = [n | n <- [1,3..m-1], gcd m n == 1]
          | otherwise = [n | n <- [2,4..m-1], gcd m n == 1]
    forM_ ns $ \n -> do
      let perimPrimitive = 2 * m * (m + n)
      let ls = [1..uBound `quot` perimPrimitive]
      forM_ ls $ \k -> do
        let perim = perimPrimitive * k
        count <- MV.read vec perim
        MV.write vec perim (count+1)
  GV.unsafeFreeze vec
