{-# LANGUAGE TupleSections #-}

module Pb23
  ( pb23
  ) where

import LibProblem

import Control.Monad.ST
import Data.Foldable
import Data.List
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

nMax :: Int
nMax = 28123

pb23 :: Computation
pb23 = answer s $ show s ++ " Σ of all naturals not a Σ of 2 abundants"
  where s = V.sum $ V.findIndices not isAbundantSum

isAbundantSum :: V.Vector Bool
isAbundantSum = runST $ do
  vec <- MV.replicate (nMax+1) False
  traverse_ (\i -> MV.unsafeWrite vec i True) $ filter (nMax >=) abundantSums
  V.unsafeFreeze vec

abundantSums :: [Int]
abundantSums = concatMap (\(a,as) -> map (+a) as) $ zip abundants (tails abundants)
  where pfSum = sum . properFactors
        abundants = [n | n <-[0..nMax], n < pfSum n]
