module Report.Command
  ( ReportCmd(..)
  , cmdReport
  ) where

import Config

-- | Class of commands that create a report.
class (Configurable cmd) => ReportCmd cmd where
  -- | Generate a report with a given configuration.
  report :: Config cmd -> IO ()

-- | Generate a report with a given list of arguments.
cmdReport :: (ReportCmd cmd) => cmd -> [String] -> IO ()
cmdReport cmd args = putParseErrs >> report cfg
  where (putParseErrs, cfg) = parseConfig cmd args
