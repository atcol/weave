{-# LANGUAGE DeriveFunctor #-}
module Data.Time.Schedule.Chaos
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
    runTarget,
    startTime,
    scheduled
    ) where

import           Control.Concurrent     (forkIO, takeMVar, threadDelay)
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
                Interval { start :: LocalTime, end :: LocalTime, tz :: TimeZone}
                -- | The schedule has passed
                | Finished
                deriving (Read, Show, Eq)

-- | The target action to be scheduled
data Target m = Target { sched :: Schedule, action :: m }
  deriving (Read, Show, Eq, Functor)

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
startTime (Interval s _ _)    = Just s

-- | Randomly pick a time compatible with the given start and end times
genTime :: (MonadIO m, RandomGen g) => Schedule -> g -> m (Maybe LocalTime, g)
genTime (Interval s e t) rg = do
  if (s < e) then return $ mapRes $ rt t
             else return (Nothing, rg)
  where rt tz = randomTimeBetween tz s e rg
        mapRes (Just (a, b)) = (Just a, b)
        mapRes Nothing       = (Nothing, rg)

diff :: TimeZone -> LocalTime -> LocalTime -> NominalDiffTime
diff tz st en = diffUTCTime (localTimeToUTC tz st) (localTimeToUTC tz en)

safeTime :: LocalTime -> LocalTime -> Bool
safeTime s e = s < e

randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg max = first realToFrac $ randomR (0, max) rg

randomTimeBetween :: RandomGen g => TimeZone -> LocalTime -> LocalTime -> g -> Maybe (LocalTime, g)
randomTimeBetween tz s e rg =
  if (s > e) then Nothing
             else Just $ first
                    (utcToLocalTime tz . (`addUTCTime` (localTimeToUTC tz s)))
                        (randomSeconds rg (abs $ floor (diff tz s e)))

runTarget :: MonadIO m => Target (m a) -> m a
runTarget (Target sc@(Interval s e tz) a) = do
  g <- liftIO $ newStdGen
  ranTime <- genTime sc g
  case ranTime of
    (Just t, _) -> do let tDiff = (round $ diff tz s t) :: Int
                          delay = abs $ tDiff * 1000 * 1000
                      liftIO $ threadDelay delay
                      a
