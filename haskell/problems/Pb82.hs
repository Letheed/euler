module Pb82
  ( pb82
  ) where

import LibIO
import LibProblem

import Prelude hiding (elem)
import Data.Matrix

import qualified Data.Text as T(Text, lines, null, tail)
import qualified Data.Text.Read as T

pb82 :: Computation
pb82 = do
  mat <- parseFile <$> readProblemT 82 "matrix"
  answer' $ minPath (fromLists mat)

minPath :: Matrix Int -> Int
minPath mat = minimum [pathMat ! (i, 1) | i <- [1..m]]
  where (m, n) = (nrows mat, ncols mat)
        downMat = matrix m n downPath -- bottom up
          where downPath (i, j)
                  | edge      = self
                  | bottom    = self + right
                  | otherwise = self + min right down
                  where (bottom, edge) = (i == m, j == n)
                        self  = mat ! (i, j)
                        right = pathMat ! (i, j+1)
                        down  = downMat ! (i+1, j)
        pathMat = matrix m n sumPath -- top down
          where sumPath (i, j)
                  | edge      = self
                  | top       = downOrRight
                  | otherwise = min up downOrRight
                  where (top, edge) = (i == 1, j == n)
                        self        = mat ! (i, j)
                        up          = self + pathMat ! (i-1, j)
                        downOrRight = downMat ! (i, j)

parseFile :: T.Text -> [[Int]]
parseFile = map parseLine . T.lines
  where parseLine txt = case T.decimal txt of
          Right (n, txt') -> n : if T.null txt' then [] else parseLine $ T.tail txt'
          Left e          -> error e
