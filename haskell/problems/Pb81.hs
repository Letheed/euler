module Pb81
  ( pb81
  ) where

import Prelude hiding (elem, sum)
import LibIO
import LibProblem
import Data.Matrix
import qualified Data.Text      as T
import qualified Data.Text.Read as T

pb81 :: Computation
pb81 = do
  mat <- parseFile <$> readProblemT 81 "matrix"
  answer' $ minPath mat
--  show $ minPath' (fromLists mat)

-- Shorter, faster (2.2 ms), some reasonning required
minPath :: [[Int]] -> Int
minPath mat = head $ foldr sumRows (sumRight lastRow) initRows
  where (initRows, lastRow) = (init mat, last mat)
        sumRight            = scanr1 (+)
        sumRows upper lower = scanr minSum (maxBound::Int) (zip upper lower)
          where minSum (elem, down) right = elem + min down right

-- More verbose, still fast (3.2 ms), very staightforward
minPath' :: Matrix Int -> Int
minPath' mat = pathMat ! (1, 1)
  where (m, n) = (nrows mat, ncols mat)
        pathMat = matrix m n sumPath
          where sumPath (i, j)
                  | bottom && edge = self
                  | bottom         = self + right
                  | edge           = self + down
                  | otherwise      = self + min down right
                  where (bottom, edge) = (i == m, j == n)
                        self  = mat ! (i, j)
                        down  = pathMat ! (i+1, j)
                        right = pathMat ! (i, j+1)

parseFile :: T.Text -> [[Int]]
parseFile = map parseLine . T.lines
  where parseLine txt
          | T.null txt = []
          | otherwise  = case T.decimal txt of
              Right (n, txt') -> n : parseLine (T.drop 1 txt')
              Left e          -> error e
