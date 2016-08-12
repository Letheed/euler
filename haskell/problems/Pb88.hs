module Pb88
  ( pb88
  ) where

import LibProblem hiding (primes)

import Batteries
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.Numbers.Primes
import Data.STRef
import qualified Data.Set as S
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

-- Upper bound for k (psNums)
uBound :: Int
uBound = 12000

pb88 :: Computation
pb88 = msgAnswer $ show . sum . dedup . map (psNums UV.!) $ [2..uBound]

psNums :: UV.Vector Int
psNums = runST $ do
  vec  <- MUV.replicate (uBound+1) 0
  let indices = S.fromDistinctAscList [2..uBound]
  vec' <- fill vec 4 indices
  UV.unsafeFreeze vec'
  where fill vec n indices
          | null indices = pure vec
          | otherwise    = do
              is <- newSTRef indices
              let productsN = products V.! n
              unless (isPrime' productsN) $ do
                let ks = filter (<= uBound) . map (compk n) $ productsN
                forM_ ks $ \k -> do
                  n' <- MUV.read vec k
                  when (n' == 0) $ do
                    modifySTRef' is (S.delete k)
                    MUV.write vec k n
              indices' <- readSTRef is
              fill vec (n+1) indices'

compk :: Int -> [(Int, Int)] -> Int
compk n productN = n - sumFactors + countFactors
  where countFactors = sum $ map snd productN
        sumFactors   = sum $ map (uncurry (*)) productN

-- Table of all the ways, for each n, to write n as a product.
-- If n is 0, 1 or prime, then n = [[(n, 1)]].
-- e.g. 8: 2³, 2 * 4 => products ! 8 = [[(2, 3)], [(2, 1), (4, 1)]]
products :: V.Vector [[(Int, Int)]]
products = V.generate (2*uBound+1) ways
  where ways n = case smallestDivisor of
          Nothing -> [[(n, 1)]]
          Just d  -> dedup $ [(d, 1), (m, 1)] : distribute d (products V.! m) where m = n `quot` d
          where smallestDivisor = listToMaybe [p | p <- takeWhile (sqrtInt n >=) primes, n `rem` p == 0]
                distribute d mWays
                  | isPrime' mWays = map (d `times`) mWays
                  | otherwise      = concatMap newWays mWays
                  where newWays mWay = d `times` mWay : map newWay mWay
                          where newWay (f, _)
                                  | p == 1    = smaller ++ times (d*f) bigger
                                  | otherwise = smaller ++ (f, p-1) : times (d*f) bigger
                                  where (smaller, (_, p):bigger) = break ((f ==) . fst) mWay


isPrime' :: [[(Int, Int)]] -> Bool
isPrime' ((f:fs):_) = null fs && snd f == 1

times :: Int -> [(Int, Int)] -> [(Int, Int)]
times n = go
  where go []         = [(n, 1)]
        go fs@((f, p):rest)
          | f == n    = (f, p+1) : rest
          | f > n     = (n, 1) : fs
          | otherwise = (f, p) : go rest
