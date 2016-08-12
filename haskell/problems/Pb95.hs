{-# LANGUAGE MultiWayIf #-}

module Pb95
  ( pb95
  ) where

import LibProblem
import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

uBound :: Int
uBound = 1000000

pb95 :: Computation
pb95 = answer' . minimum . fst . maximumBy (comparing snd) . map ((id &&& length) . buildCycle) $ cycleHeads

buildCycle :: Int -> [Int]
buildCycle n = n : takeWhile (n /=) (iterate follow (follow n))

cycleHeads :: [Int]
cycleHeads = runST $ do
  record <- MV.replicate (uBound+1) 0
  catMaybes <$> mapM (cycleHead record) [1..uBound]

cycleHead :: MV.MVector s Int -> Int -> ST s (Maybe Int)
cycleHead record chainhead = test chainhead
  where test n = if n < chainhead || n > uBound
                 then pure Nothing
                 else do chaintag <- MV.read record n
                         if | chaintag == 0         -> do MV.write record n chainhead
                                                          test (follow n)
                            | chaintag == chainhead -> pure (Just n)
                            | otherwise             -> pure Nothing

follow :: Int -> Int
follow = (sumProperDivisorsVec V.!)

sumProperDivisorsVec :: V.Vector Int
sumProperDivisorsVec = runST $ do
  vec <- MV.replicate (uBound+1) 1
  forM_ [2..uBound `quot` 2] $ \n ->
    forM_ [2..uBound `quot` n] (\i -> MV.unsafeModify vec (+n) (i*n))
  V.unsafeFreeze vec
