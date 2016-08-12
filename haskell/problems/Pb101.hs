module Pb101
  ( pb101
  ) where

import LibProblem
import qualified Data.Eigen.Matrix as M

default (Int)

pb101 :: Computation
pb101 = answer' . sum . map (fit . bop) $ [1..10]

bop :: Int -> [Int]
bop k = map v [1..]
  where mat   = M.generate k k (\i j -> fromIntegral (i+1)^(k-j-1))  :: M.MatrixXd
        vec   = M.generate k 1 (\i _ -> fromIntegral (u (i+1)))      :: M.MatrixXd
        coefs = M.inverse mat * vec
        v n   = sum . zipWith (*) (map round . concat . M.toList $ coefs) . map (n^) $ [k-1, k-2..0]

fit :: [Int] -> Int
fit = snd . head . dropWhile (uncurry (==)) . zip useq
  where useq = map u [1..]

u :: Int -> Int
u n = sum . zipWith (*) altSign . map (n^) $ [0..10]
  where altSign = 1 : (-1) : altSign
