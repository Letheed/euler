module Math
  ( module MathExports
  , SpecializedIntegral(..)
  , concatDigits, concatRevDigits, concatNum
  , isPalindromic
  -- , isPandigital
  -- , is9Pandigital
  , even', odd'
  , sqrtInt
  , factorial
  , factors
  , nFactors
  , properFactors
  , choose
  , toNum
  , maxTrianglePathSum
  , fibs
  , collatz, collatzSeq
  , polygonals
  , PTriple, PTree(..), pTree
  ) where

import Math.NumberTheory.Powers.Squares   as MathExports (isSquare, isSquare')
import Math.Primes                        as MathExports

import Batteries
import Data.Bits
import Data.Char
import Data.List

class SpecializedIntegral a where
  numLen     :: a -> Int
  factorials :: [a]
  mirrorNum  :: a -> a
  digitSum   :: a -> Int

instance SpecializedIntegral Int where
  numLen n = if n < 10 then 1 else 1 + numLen (n `quot` 10)
  factorials = takeWhile (0 <) $ scanl (*) 1 [1..]
  mirrorNum n
    | n < 0     = - (concatRevDigits . map digitToInt . tail . show $ n)
    | otherwise =    concatRevDigits . map digitToInt . show $ n
  digitSum n
    | n < 0     = sum . map digitToInt . tail . show $ n
    | otherwise = sum . map digitToInt . show $ n

instance SpecializedIntegral Integer where
  numLen = length . show
  factorials = scanl (*) 1 [1..]
  mirrorNum n
    | n < 0     = - (concatRevDigits . map (toInteger . digitToInt) . tail . show $ n)
    | otherwise =    concatRevDigits . map (toInteger . digitToInt) . show $ n
  digitSum n
    | n < 0     = sum . map digitToInt . tail . show $ n
    | otherwise = sum . map digitToInt . show $ n

concatDigits :: (Integral a) => [a] -> a
{-# SPECIALIZE concatDigits :: [Int] -> Int #-}
{-# SPECIALIZE concatDigits :: [Integer] -> Integer #-}
concatDigits = foldl1 (\n d -> n * 10 + d)

concatRevDigits :: (Integral a) => [a] -> a
{-# SPECIALIZE concatDigits :: [Int] -> Int #-}
{-# SPECIALIZE concatDigits :: [Integer] -> Integer #-}
concatRevDigits = foldr1 (\d n -> n * 10 + d)

concatNum :: (Integral a) => a -> a -> a
{-# SPECIALIZE concatNum :: Int -> Int -> Int #-}
{-# SPECIALIZE concatNum :: Integer -> Integer -> Integer #-}
concatNum x y = x * shiftl10 + y
  where shiftl10 = until (y <) (10 *) 1

isPalindromic :: (Show a) => a -> Bool
{-# SPECIALIZE isPalindromic :: Int -> Bool #-}
{-# SPECIALIZE isPalindromic :: Integer -> Bool #-}
isPalindromic n = str == reverse str
  where str = show n

-- isPandigital :: (Integral a, Show a) => a -> Bool
-- isPandigital n = n <= 987654321 && pandigital
--   where pandigital = all (uncurry (==)) . zip ['1'..'9'] . sort . show $ n

-- is9Pandigital :: (Integral a, Show a) => a -> Bool
-- is9Pandigital n = n >= 123456789 && isPandigital n

even' :: (Num a, Bits a) => a -> Bool
{-# SPECIALIZE even' :: Int -> Bool #-}
{-# SPECIALIZE even' :: Integer -> Bool #-} -- don't know how it compares to `even`
even' n = n .&. 1 == 0

odd' :: (Num a, Bits a) => a -> Bool
{-# SPECIALIZE odd' :: Int -> Bool #-}
{-# SPECIALIZE odd' :: Integer -> Bool #-} -- don't know how it compares to `odd`
odd' n = n .&. 1 == 1

sqrtInt :: (Integral a) => a -> a
{-# SPECIALIZE sqrtInt :: Int -> Int #-}
{-# SPECIALIZE sqrtInt :: Integer -> Integer #-}
sqrtInt = floor . sqrt . fromIntegral

factorial :: (Integral a) => a -> a
{-# SPECIALIZE factorial :: Int -> Int #-}
{-# SPECIALIZE factorial :: Integer -> Integer #-}
factorial n = product [2..n]

factors :: Int -> [Int]
factors n
  | n < 2     = [1 | n == 1]
  | otherwise = foldr1 (\pm -> (((*) <$> pm) <*>)) primeMultiples
  where primeMultiples = map (map product . inits) . group . primeFactors $ n

nFactors :: Int -> Int
nFactors = product . map ((+1) . length) . group . primeFactors

properFactors :: Int -> [Int]
properFactors n
  | n == 0    = []
  | otherwise = init (factors n)

choose :: (Integral a) => a -> a -> a
{-# SPECIALIZE choose :: Int -> Int -> Int #-}
{-# SPECIALIZE choose :: Integer -> Integer -> Integer #-}
choose n k = product [n-k'+1..n] `quot` product [2..k']
  where k' = min k (n-k)

toNum :: (Num a) => [a] -> a
{-# SPECIALIZE toNum :: [Int] -> Int #-}
{-# SPECIALIZE toNum :: [Integer] -> Integer #-} -- would read be more efficient ?
toNum = foldl1' (\n d -> n * 10 + d)             -- if so, move to SpecializedIntegral

maxTrianglePathSum :: [[Int]] -> Int
maxTrianglePathSum = headThrow errMsg . foldr1 flatten
  where flatten upper lower = zipWith (+) upper (idealize lower)
        idealize row = case row of
          (a:rest@(b:_)) -> max a b : idealize rest
          [a]            -> [a]
          []             -> error "maxTrianglePath: unreachable"
        errMsg = "maxTrianglePathSum: empty list"

fibs :: (Integral a) => [a]
{-# SPECIALIZE fibs :: [Int] #-}
{-# SPECIALIZE fibs :: [Integer] #-}
fibs = 0:1:zipWith (+) fibs (tail fibs)

polygonals :: (Integral a) => a -> [a]
{-# SPECIALIZE polygonals :: Int -> [Int] #-}
{-# SPECIALIZE polygonals :: Integer -> [Integer] #-}
polygonals p = scanl1 (+) [1, p-1..]

collatz :: Int -> Int
collatz n
  | even' n   = n `quot` 2
  | otherwise = n * 3 + 1

collatzSeq :: Int -> [Int]
collatzSeq n = n : ns
  where ns
          | n == 1    = []
          | otherwise = collatzSeq (collatz n)

type PTriple = (Int, Int, Int)

data PTree = Node PTriple PTree PTree PTree

pTree :: PTree
pTree = genNode (2, 1)
  where genNode (m, n) = Node triple child1 child2 child3
          where triple = (a, b, c)
                  where a = m * m - n * n
                        b = 2 * m * n
                        c = m * m + n * n
                child1 = genNode (2*m-n, m)
                child2 = genNode (2*m+n, m)
                child3 = genNode (m+2*n, n)
