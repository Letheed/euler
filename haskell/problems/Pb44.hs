{-# LANGUAGE TupleSections #-}

module Pb44
  ( pb44
  ) where

import LibProblem

import Batteries
import Data.Array.Unboxed
import Data.List
import Data.Maybe

pb44 :: Computation
pb44 = answer d $ show p ++ " pentagonals, Σ and diff as well, diff minimized"
  where d = pk - pj
        p@(pj, pk) = head pentaPairs -- only 1 solution in pentaLst


pentaPairs :: [(Int, Int)]
pentaPairs = concatMap go . tails $ pentaLst
  where go []     = []
        go (j:ks) = mapMaybe pentaPair ks
          where pentaPair k
                  | sumIsPenta && diffIsPenta = Just (j, k)
                  | otherwise                 = Nothing
                  where sumIsPenta  = isPentaNum (k + j)
                        diffIsPenta = isPentaNum (k - j)

isPentaNum :: Int -> Bool
isPentaNum = (pentaArray !)
  where pentaArray :: UArray Int Bool
        pentaArray = accumArray (const2 True) False (0, 2 * last pentaLst) $ map (, ()) pentaLst

pentaLst :: [Int]
pentaLst = take 2500 (polygonals 5)
