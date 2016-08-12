module Math.Primes
  ( primes, isPrime
  , primeFactors, primeDivisors
  , nDistinctPF
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.List
import Data.Maybe
--import qualified Data.Numbers.Primes
import qualified Data.Vector.Unboxed as V hiding((++))
import qualified Data.Vector.Unboxed.Mutable as MV

pMax :: Int
pMax = 2000000

primes :: [Int]
primes = V.toList $ V.findIndices id primeVec

isPrime :: Int -> Bool
isPrime n
  | n <= pMax = primeVec V.! n
  | otherwise = primeFactors n == [n]

primeVec :: V.Vector Bool
primeVec = crossOutFrom 3 initSieve
  where sieve0 = V.generate (pMax+1) (const True)
        initSieve = sieve0 V.// ([(0, False), (1, False)] ++ [(n, False) | n <- [4, 6..pMax]])
        crossOutFrom p sieve
          | pSqr > pMax = sieve
          | otherwise   = case nextPrime of
              Nothing -> updatedSieve
              Just q  -> crossOutFrom q updatedSieve
          where pSqr = p * p
                updatedSieve = sieve V.// [(i, False) | i <- [pSqr, pSqr + 2*p..pMax]]
                nextPrime = listToMaybe [i | i <- [p+2, p+4..pMax], updatedSieve V.! i]

primeFactors :: Int -> [Int]
primeFactors n
  | n < 2     = []
  | otherwise = pFactors n primes
  where pFactors m ps@(p:rest)
          | m < p * p = [m]
          | r == 0    = p : pFactors q ps
          | otherwise = pFactors m rest
          where (q, r) = quotRem m p
        pFactors _ [] = error "primeFactors: ran out of primes"

primeDivisors :: Int -> [Int]
primeDivisors = map head . group . primeFactors

nDPFMax :: Int
nDPFMax = 150000

nDistinctPF :: Int -> Int
nDistinctPF = (nDPFVec V.!)

-- nDistinctPF :: Int -> Int
-- nDistinctPF n
--   | n <= nDPFMax = nDPFVec V.! n
--   | otherwise    = length . dedup . primeFactors $ n

nDPFVec :: V.Vector Int
nDPFVec = runST $ do
  vec <- MV.replicate (nDPFMax+1) 0
  for_ [2..nDPFMax `quot` 2] $ \i -> do
    nDPF <- MV.unsafeRead vec i
    when (nDPF == 0) $ traverse_ (MV.unsafeModify vec (+1)) [2*i, 3*i..nDPFMax]
  V.unsafeFreeze vec
