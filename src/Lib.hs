{-# LANGUAGE DeriveFunctor #-}
module Lib
  (
    -- | Types
    Schedule (..),
    Target (..),
    Bound (..),

    -- | Functions
    genTime,
    getSchedule,
    randomSeconds,
    randomTimeBetween,
    result,
    startTime,
    scheduled
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bifunctor         (first)
import           Data.Time.Clock        (NominalDiffTime, UTCTime, addUTCTime,
                                         diffUTCTime, getCurrentTime)
import           Data.Time.LocalTime    (LocalTime, TimeZone,
                                         getCurrentTimeZone, localTimeToUTC,
                                         utcToLocalTime)
import           System.Random          (Random (..), RandomGen, newStdGen,
                                         randomR)

data Bound = Upper | Lower deriving (Show, Eq, Ord)

-- | The scheduling type, representing when an action should occur, and within which bounds
data Schedule = -- | Perform something within the start and end times
                Interval { start :: LocalTime, end :: LocalTime }
                -- | Begin after, or by, the given time
                | Bounded { time :: LocalTime, bound :: Bound }
                | Finished
                deriving (Show, Eq)

-- | The target action to be scheduled
data Target m = Target { sched :: Schedule, action :: m }
              deriving (Show, Eq, Functor)

-- | Retrieve the required schedule
getSchedule :: MonadIO m => Target (m a) -> Maybe Schedule
getSchedule (Target s _) = Just s

-- | Construct an schedule with the specified action
scheduled :: MonadIO m => m a -> Schedule -> Target (m a)
scheduled ioa s = Target s ioa

-- | Pull the result from the target
result :: MonadIO m => Target (m a) -> m a
result (Target _ a) = a

startTime :: Schedule -> Maybe LocalTime
startTime (Interval s _)    = Just s
startTime (Bounded s Lower) = Just s
startTime _                 = Nothing

-- | Randomly pick a time compatible with the given start and end times
genTime :: (MonadIO m, RandomGen g) => Schedule -> g -> m (Either String LocalTime)
genTime (Bounded t b) rg = do
  tz <- liftIO $ getCurrentTimeZone
  now <- liftIO $ getCurrentTime
  let diff = diffUTCTime now (localTimeToUTC tz t)
  case b of
    Upper -> if beforeNow t tz now then return $ Left "Upper bound time is before now"
                                   else genTime (Interval (ltNow tz now) t) rg
    Lower -> genTime (Interval t (ltNow tz now)) rg
  where ltNow tz n = utcToLocalTime tz n
genTime (Interval s e) rg = do
  tz <- liftIO $ getCurrentTimeZone
  now <- liftIO $ getCurrentTime
  if safeTime s e then return $ Right $ rt tz
                  else return $ Left "Start is < or == to end"
  where rt tz = fst $ randomTimeBetween tz s e rg

beforeNow :: LocalTime -> TimeZone -> UTCTime -> Bool
beforeNow e tz now = e < (utcToLocalTime tz now)

diff :: TimeZone -> LocalTime -> LocalTime -> NominalDiffTime
diff tz st en = diffUTCTime (localTimeToUTC tz st) (localTimeToUTC tz en)

safeTime :: LocalTime -> LocalTime -> Bool
safeTime s e = (s < e) && (s /= e)

randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg max = first realToFrac $ randomR (0, max) rg

randomTimeBetween :: RandomGen g => TimeZone -> LocalTime -> LocalTime -> g -> (LocalTime, g)
randomTimeBetween tz s e rg =
  first
      (utcToLocalTime tz . (`addUTCTime` (localTimeToUTC tz s)))
          (randomSeconds rg (floor (diff tz s e)))
