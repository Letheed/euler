module LibIO
  ( readProblem, readProblemT
  ) where

import System.IO.Batteries
import System.Process
import Text.Printf
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import GHC.IO.Handle

-- | Show an integer with a minimum field width of 4
-- and leading zeros.
show04d :: Int -> String
show04d = printf "%04d"

-- | Resolve the absolute path to the problems directory.
-- We assume we are in a git repository.
getProblemsDir :: IO String
getProblemsDir = do
  let (cmd, args) = ("git", ["rev-parse", "--show-toplevel"])
  let p = (proc cmd args) { std_out = CreatePipe }
  (_, Just hout, _, _) <- createProcess p
  rootDir <- hGetLine hout
  pure $ mkPath [rootDir, "problems"]

mkPbReader :: (String -> IO a) -> (Int -> String -> IO a)
mkPbReader readFileFunction = \n fileName -> do
  pbDir <- getProblemsDir
  readFileFunction $ mkPath [pbDir, show04d n, fileName]

-- | Read a problem input file using `readFile`.
readProblem :: Int -> String -> IO String
readProblem = mkPbReader readFile

-- | Read a problem input file using `Text.readFile`.
readProblemT :: Int -> String -> IO T.Text
readProblemT = mkPbReader T.readFile
