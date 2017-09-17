module Lib
  (
    -- | Types
    Schedule (..),
    Target (..),
    Bound (..),

    -- | Functions
    immediate,
    scheduled
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Time.LocalTime    (LocalTime)

data Bound = Upper | Lower deriving (Show, Eq, Ord)

-- | The scheduling type, representing when an action should occur, and within which bounds
data Schedule =
              -- | Perform something within the start and end times
                Interval { start :: LocalTime, end :: LocalTime }
              |
                -- | Begin after, or by, the given time
                Bounded { time :: LocalTime, bound :: Bound, withinMs :: Maybe Int }
              deriving (Show, Eq)

-- | The target action to be scheduled
data Target m = Immediate { action :: m }
              | Scheduled { sched :: Schedule, action :: m }
              deriving (Show, Eq)

immediate :: MonadIO m => m a -> Target (m a)
immediate ioa = Immediate ioa

scheduled :: MonadIO m => m a -> Schedule -> Target (m a)
scheduled ioa s = Scheduled s ioa
