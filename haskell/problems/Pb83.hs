module Pb83
  ( pb83
  ) where

import LibIO
import LibProblem

import Prelude hiding (elem, sum)
import Data.List (sortOn)
import Data.Matrix

import qualified Data.Text as T(Text, lines, null, tail)
import qualified Data.Text.Read as T

pb83 :: Computation
pb83 = do
  mat <- parseFile <$> readProblemT 83 "matrix"
  answer' $ minPath (fromLists mat)

minPath :: Matrix Int -> Int
-- minPath mat = pathMat ! (1, 1)
minPath mat = path
  where (m, n) = (nrows mat, ncols mat)
        -- pathMat — Complete solution. Yields the complete matrix of minimum paths.
        -- Takes more time (~325 ms)
        pathMat = go initMat
          where go oldMat
                  | oldMat == newMat = newMat
                  | otherwise        = go newMat
                  where newMat  = update oldMat
        -- path — Partial solution. Focuses on getting the diagonal path.
        -- Much faster (25 ms), depending on how sure we want to be that the optimal solution was reached.
        -- `n` is a counter for the number of consecutive identical values of the path.
        -- The end value (here 3) decides how certain we want to be that we reached the solution.
        path = go initMat 0 0
          where go oldMat oldPath n
                  | n' == 3   = newPath
                  | otherwise = go newMat newPath n'
                  where newMat  = update oldMat
                        newPath = sumPath mat newMat
                        n'
                          | oldPath == newPath = n + 1
                          | otherwise          = 0
        initMat = matrix m n initPath
          where initPath (i, j)
                  | bottom && right = self
                  | otherwise       = self + minimum candidates
                  where (bottom, right)    = (i == m, j == n)
                        (self, candidates) = (mat ! (i, j), map (initMat !) neighbours)
                        neighbours = [(i', j') | (i', j') <- [(i+1, j), (i, j+1)], i' <= m, j' <= n]
        update prevMat = matrix m n updatePath
          where updatePath (i, j) = min prev (self + minimum candidates)
                  where (self, prev) = (mat ! (i, j), prevMat ! (i, j))
                        candidates   = map (prevMat !) neighbours
                        neighbours   = [(i', j') | (i', j') <- [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
                                                 , i' > 0, j' > 0, i' <= m, j' <= n]

-- Compute the diagonal path sum value (with (1, 1) the max value and (m, n) the min)
sumPath :: Matrix Int -> Matrix Int -> Int
sumPath mat pathMat = go (1, 1) (0, 0) 0
  where (m, n) = (nrows mat, ncols mat)
        go (i, j) (iprev, jprev) sum
          | bottom && right = sum'
          | otherwise       = go next (i, j) sum'
          where (bottom, right) = (i == m, j == n)
                sum' = sum + mat ! (i, j)
                next = head . sortOn (pathMat !) $ candidates
                candidates = [(i', j') | (i', j') <- [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
                                       , i' > 0, j' > 0, i' <= m, j' <= n, i' /= iprev || j' /= jprev]

parseFile :: T.Text -> [[Int]]
parseFile = map parseLine . T.lines
  where parseLine txt = case T.decimal txt of
          Right (n, txt') -> n : if T.null txt' then [] else parseLine $ T.tail txt'
          Left e          -> error e
