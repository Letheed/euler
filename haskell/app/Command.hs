module Command
  ( Command(..)
  ) where

-- | Class of types representing commands.
class Command cmd where
  -- | Returns the name of a command.
  cmdName :: cmd -> String
