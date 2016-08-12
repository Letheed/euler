{-# LANGUAGE PatternSynonyms #-}

module Args.Options
  ( Option(Option)
  , isOptionS
  , parseOptionsS
  , warnUnknownOptionS
  ) where

import Command

import Batteries
import Control.Arrow
import Data.List

-- | Option string and optionnal value.
data Option = Opt String (Maybe String)
            deriving (Eq, Ord)

pattern Option :: String -> Maybe String -> Option
pattern Option opt mval <- Opt opt mval

instance Show Option where
  show (Opt opt  Nothing)   = show  opt
  show (Opt opt (Just val)) = show (opt ++ '=':val)

-- | Option predicate for `String`.
isOptionS :: String -> Bool
isOptionS = ("-" `isPrefixOf`)

-- | Parse a list of arguments as a list of options.
--
-- Produces an error if one of the arguments
-- is not an option or a list of options.
parseOptionsS :: [String] -> [Option]
parseOptionsS = concatMap parseOptionS

-- | Parse a `String` as an option or a list of short options.
--
-- Produces an error if the string is not an option.
parseOptionS :: String -> [Option]
parseOptionS arg
  | longOption  = [Opt (toLowerS opt) mval]
  | shortOption = go (drop 1 opt)
  | otherwise   = error "processOption: string is not an option"
  where longOption  = "--" `isPrefixOf` opt
        shortOption = "-"  `isPrefixOf` opt && not longOption
        (opt, val)  = second (drop 1) . break ('=' ==) $ arg
        mval        = if null val then Nothing else Just val
        go []     = [Opt  "-"      mval]
        go [o]    = [Opt ('-':[o]) mval]
        go (o:os) =  Opt ('-':[o]) Nothing : go os

-- | Print a warning for a list of unrecognized otions.
warnUnknownOptionS :: (Command cmd) => cmd -> [Option] -> IO ()
warnUnknownOptionS _   []    = pure ()
warnUnknownOptionS cmd [opt] = putWarnS $ cmdName cmd ++ ": unknown option " ++ show opt
warnUnknownOptionS cmd opts  = putWarnS $ cmdName cmd ++ ": unknown options [" ++ optionsList ++ "]"
  where optionsList = intercalate "," (show <$> dedup opts)
