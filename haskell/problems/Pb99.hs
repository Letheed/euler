module Pb99
  ( pb99
  ) where

import LibIO
import LibProblem
import Data.List
import Data.Ord
import qualified Data.Text      as T
import qualified Data.Text.Read as T

pb99 :: Computation
pb99 = do
  base_exps <- zip [(1::Int)..] . map parseLine . T.lines <$> readProblemT 99 "base_exps"
  let (n, (b, e)) = maximumBy (comparing (\(_, (b, e)) -> e * log b)) base_exps
  msgAnswer $ "line " ++ show n ++ " (" ++ show (round b) ++ "^" ++ show (round e) ++ ")"

parseLine :: T.Text -> (Double, Double)
parseLine line = (parse t0, parse (T.drop 1 t1))
  where (t0, t1) = T.break (== ',') line
        parse t = fromIntegral (n::Int)
          where Right (n, _) = T.decimal t
