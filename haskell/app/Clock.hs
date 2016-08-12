{-# LANGUAGE ViewPatterns #-}

module Clock
  ( TimeSpec
  , DurationType(..), selectDur
  , TimeSpecs, getTimeSpecs
  , diffTs, quotTs
  , showDuration, showDurations
  ) where

import Present

import Batteries
import System.Clock
import Text.Printf

infixl 7 `quotRound`
infixl 7 `quotFloat`

-- | Type of clock used for measuring time|durations.
data DurationType = Thread
                  | Process
                  deriving (Eq)

-- | Time record for all supported clocks.
data TimeSpecs = TimeSpecs
  { thread   :: TimeSpec
  , process  :: TimeSpec
  }

instance Monoid TimeSpecs where
  mempty = TimeSpecs t0 t0 where t0 = TimeSpec 0 0
  mappend (TimeSpecs t1 p1) (TimeSpecs t2 p2) = TimeSpecs (t1 + t2) (p1 + p2)

-- | Getter for any choosen clock in a time record.
selectDur :: DurationType -> TimeSpecs -> TimeSpec
selectDur Thread  = thread
selectDur Process = process

-- | Subtraction for time records.
diffTs :: TimeSpecs -> TimeSpecs -> TimeSpecs
diffTs ts1 ts0 = TimeSpecs
  { thread  = thread  ts1 - thread  ts0
  , process = process ts1 - process ts0
  }

-- | Divide all clocks in a time record by an integer.
quotTs :: (Integral a) => TimeSpecs -> a -> TimeSpecs
quotTs ts n = TimeSpecs
  { thread  = thread  ts `quotT` n
  , process = process ts `quotT` n
  }

-- | Divide a clock record by an integer.
quotT :: (Integral a) => TimeSpec -> a -> TimeSpec
quotT (toNanoSecs -> t) n = fromNanoSecs (t `quot` toInteger n)

-- | Make a time record for all supported clocks.
getTimeSpecs :: IO TimeSpecs
getTimeSpecs = do
  t <- getTime ThreadCPUTime
  p <- getTime ProcessCPUTime
  pure TimeSpecs { thread = t, process = p }

-- | Show any choosen duration from a time record.
--
-- Uses ANSI strings for color. Suitable for terminals only.
showDuration :: DurationType -> TimeSpecs -> String
showDuration durType = showTimeSpec . selectDur durType

-- | Show several durations from a time record.
--
-- Uses ANSI strings for color. Suitable for terminals only.
showDurations :: [DurationType] -> TimeSpecs -> [String]
showDurations durTypes durs = showDuration <$> durTypes <*> pure durs

-- | Show a clock record as a duration.
--
-- Uses ANSI strings for color. Suitable for terminals only.
showTimeSpec :: TimeSpec -> String
showTimeSpec (TimeSpec s ns)
  | s >= 10   = toRed     $ show3d s ++ " s "
  | s /= 0    = toRed     $ show s ++ "." ++ show (ns `quotRound` 10^8) ++ " s "
  | ns < 10^3 = toMagenta $ show3d ns ++ " ns"
  | ns < 10^6 = toMagenta $ show_ns 3 ++ " μs"
  | ns < 10^8 = toBlue    $ show_ns 6 ++ " ms"
  | otherwise = toYellow  $ show3d (ns `quotRound` 10^6) ++ " ms"
  where show_ns order
          | t >= 10   = show3d (round t)
          | otherwise = printf "%.1f" t
          where t = ns `quotFloat` 10^order

quotRound :: (Integral a) => a -> Double -> Integer
quotRound n d = round (n `quotFloat` d)

quotFloat :: (Integral a) => a -> Double -> Double
quotFloat n d = fromIntegral n / d
