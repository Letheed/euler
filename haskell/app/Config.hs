{-# LANGUAGE TypeFamilies #-}

module Config
  ( Configurable(..)
  ) where

import Args.Options
import Args.PbIds
import Command

import Control.Arrow
import Data.Kind
import Data.List

-- | Class for commands that can be configured.
class (Command cmd) => Configurable cmd where
  -- | Configuration data.
  data Config cmd :: Type

  -- | Default configuration.
  defaultConfig   :: Config cmd
  -- | Parse a list of arguments into a configuration and an IO action for warnings/errors.
  parseConfig     :: cmd -> [String] -> (IO (), Config cmd)
  -- | Parse an argument.
  -- Return the updated configuration if the parsing succeeded and `Nothing` otherwise.
  parseArg        :: Config cmd -> String -> Maybe (Config cmd)
  -- | Parse an option.
  -- Return the updated configuration if the parsing succeeded and `Nothing` otherwise.
  parseOption     :: Config cmd -> Option -> Maybe (Config cmd)

  parseConfig cmd allArgs = (parseErrIO, cfgFull)
    where (opts, args)       = first parseOptionsS . partition isOptionS $ allArgs
          (badOpts, cfgOpts) = parseOptions defaultConfig opts
          (badArgs, cfgFull) = parseArgs cfgOpts args
          parseErrIO         = warnNotPbIdS badArgs >> warnUnknownOptionS cmd badOpts

-- | Parse a list of options.
-- Return the updated configuration and the faulty options.
parseOptions :: (Configurable cmd) => Config cmd -> [Option] -> ([Option], Config cmd)
parseOptions cfg = foldl' (handleBadArg parseOption) ([], cfg)

-- | Parse a list of arguments.
-- Return the updated configuration and the faulty arguments.
parseArgs :: (Configurable cmd) => Config cmd -> [String] -> ([String], Config cmd)
parseArgs cfg = foldl' (handleBadArg parseArg) ([], cfg)

-- | Feed an argument to a parser.
-- Return with the updated configuration or append the argument to the list of faulty argements.
handleBadArg :: (cfg -> arg -> Maybe cfg) -> ([arg], cfg) -> arg -> ([arg], cfg)
handleBadArg parser (badArgs, cfg) arg = case parser cfg arg of
  Nothing     -> (badArgs ++ [arg], cfg)
  Just newCfg -> (badArgs, newCfg)
