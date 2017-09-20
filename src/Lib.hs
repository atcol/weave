module Lib
  (
    -- | Types
    Schedule (..),
    Target (..),
    Bound (..),

    -- | Functions
    immediate,
    genTime,
    getSchedule,
    randomSeconds,
    result,
    scheduled
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Time.Clock        (NominalDiffTime, addUTCTime,
                                         diffUTCTime)
import           Data.Time.LocalTime    (LocalTime, TimeZone, localTimeToUTC,
                                         utcToLocalTime)
import           System.Random          (RandomGen, newStdGen, randomR)

data Bound = Upper | Lower deriving (Show, Eq, Ord)

-- | The scheduling type, representing when an action should occur, and within which bounds
data Schedule = -- | Perform something within the start and end times
                Interval { start :: LocalTime, end :: LocalTime }
                | -- | Begin after, or by, the given time
                Bounded { time :: LocalTime, bound :: Bound }
              deriving (Show, Eq)

-- | The target action to be scheduled
data Target m = Immediate { action :: m }
              | Scheduled { sched :: Schedule, action :: m }
              deriving (Show, Eq)

-- | Retrieve the required schedule
getSchedule :: MonadIO m => Target (m a) -> Maybe Schedule
getSchedule (Immediate _)   = Nothing
getSchedule (Scheduled s _) = Just s

immediate :: MonadIO m => m a -> Target (m a)
immediate ioa = Immediate ioa

scheduled :: MonadIO m => m a -> Schedule -> Target (m a)
scheduled ioa s = Scheduled s ioa

result :: MonadIO m => Target (m a) -> m a
result (Immediate a)   = a
result (Scheduled _ a) = a

-- | Randomly pick a time compatible with the given schedule, using the random gen provided
genTime :: RandomGen g => TimeZone -> Schedule -> g -> LocalTime
genTime tz (Interval st en) rg = utcToLocalTime tz (randTime st)
  where randTime st = addUTCTime randStart (localTimeToUTC tz st)
        -- FIXME the following is awful
        diff = floor (diffUTCTime (localTimeToUTC tz st) (localTimeToUTC tz en))
        randStart = randomSeconds rg diff

randomSeconds :: RandomGen g => g -> Int -> NominalDiffTime
randomSeconds rg max = realToFrac (fst (randomR (0, max) rg))
