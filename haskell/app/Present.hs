module Present
  ( verticalSeparator, columns, putColumns
  , pbHeader, printRunningHeader
  , toErrStatus, show3d, showpc
  ) where

import Problems

import Data.List
import Data.List.Batteries
import Data.String.ANSI
import System.IO
import Text.Printf


-- | Separator between columns.
verticalSeparator :: String
verticalSeparator = " "

-- | Show a list of strings as columns.
columns :: [String] -> String
columns = intercalate verticalSeparator . filter some

-- | Print a list of strings as columns.
putColumns :: [String] -> IO ()
putColumns = putStrLn . columns

-- | Problem header.
--
-- Uses ANSI strings for color. Suitable for terminals only.
pbHeader :: PbId -> String
pbHeader i = toCyan ("problem " ++ show3d i)

-- | Print a temporary problem header to stdout.
-- The header will be erased the next time the cache is flushed.
--
-- Uses ANSI strings for color. Suitable for terminals only.
printRunningHeader :: String -> PbId -> IO ()
printRunningHeader msg i = do
  let header_msg = toYellow msg ++ (' ' : pbHeader i)
  putStr header_msg
  hFlush stdout
  putStr $ "\ESC[2K\ESC[" ++ show (length header_msg) ++ "D"

-- | Turn a boolean into a Ok/Err status message.
--
-- Uses ANSI strings for color. Suitable for terminals only.
toErrStatus :: Bool -> String
toErrStatus True  = toGreen "Ok "
toErrStatus False = toRed "Err"

-- | Show a integer with a minimum field width of 3.
show3d :: (Integral a) => a -> String
show3d = printf "%3d" . toInteger

-- | Show a float as a percentage,
-- with a minimum field width of 5 and a precision of 1.
showpc :: Float -> String
showpc f = printf "%5.1f" f ++ "%"
