module Pb22
  ( pb22
  ) where

import LibIO
import LibProblem

import Batteries
import Data.List
import qualified Data.Text as T

pb22 :: Computation
pb22 = do
  names <- sort . splitQuotedListT <$> readProblemT 22 "names"
  let s = sum . zipWith (*) [1..] . map (alphaOrdS . T.unpack) $ names
  answer s $ show s ++ " Σ of all the name scores"
