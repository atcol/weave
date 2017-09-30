{-# LANGUAGE DeriveFunctor #-}
module Data.Time.Schedule.Chaos
  (
    Bound (..),
    Frequency (..),
    Schedule (..),
    Strategy (..),
    Target (..),

    -- | Functions
    genTime,
    getSchedule,
    nTimes,
    randomSeconds,
    randomTimeBetween,
    result,
    runTarget,
    startTime,
    scheduled
    ) where

import           Control.Concurrent     (forkIO, takeMVar, threadDelay)
import           Control.Monad          (liftM, replicateM, replicateM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bifunctor         (first)
import           Data.Time.Clock        (NominalDiffTime, UTCTime, addUTCTime,
                                         diffUTCTime, getCurrentTime)
import           Data.Time.LocalTime    (LocalTime, TimeZone,
                                         getCurrentTimeZone, localTimeToUTC,
                                         utcToLocalTime)
import           System.Random          (Random (..), RandomGen, StdGen,
                                         newStdGen, randomR)

-- | The scheduling type, representing when an action should occur, and within which bounds
data Schedule = -- | Perform something within the start and end times
              Interval { start :: LocalTime, end :: LocalTime, tz :: TimeZone }
              -- | The schedule has passed
              | Finished
              deriving (Read, Show, Eq)

-- | The target action to be scheduled
data Target m = Target { sched :: Schedule, action :: m }
  deriving (Read, Show, Eq, Functor)

data Bound = Upper | Lower deriving (Show, Eq, Read, Ord)

data Strategy = -- | Execute this many times
                Exactly Int
                -- | Execute n times in the interval (min, max)
                | Randomly { min :: Int, max :: Int }
                deriving (Read, Show, Eq)

data Frequency = Frequency Int
                  deriving (Read, Show, Eq)

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

-- | Provide the difference between two LocalTime instances
diff :: TimeZone -> LocalTime -> LocalTime -> NominalDiffTime
diff tz st en = diffUTCTime (localTimeToUTC tz st) (localTimeToUTC tz en)

-- | Is left before right?
safeTime :: LocalTime -> LocalTime -> Bool
safeTime s e = s < e

-- | Generate seconds in the interval (0, n)
randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg max = first realToFrac $ randomR (0, max) rg

-- | Generate time within the given Timezone and boundary
randomTimeBetween :: RandomGen g => TimeZone -> LocalTime -> LocalTime -> g -> Maybe (LocalTime, g)
randomTimeBetween tz s e rg =
  if (s > e) then Nothing
             else Just $ first
                    (utcToLocalTime tz . (`addUTCTime` (localTimeToUTC tz s)))
                        (randomSeconds rg (abs $ floor (diff tz s e)))

-- | Randomly execute the given target within its schedule boundary
runTarget :: RandomGen g => Target (IO a) -> g -> IO a
runTarget (Target sc a) g = delayFor sc g >> a

-- | Delay for a random amount within the schedule
delayFor :: RandomGen g => Schedule -> g -> IO ()
delayFor sc@(Interval s e tz) g = do
  ranTime <- genTime sc g
  case ranTime of
    (Just t, _) -> do let tDiff = (round $ diff tz s t) :: Int
                          delay = abs $ tDiff * 1000 * 1000
                      liftIO $ threadDelay delay
    _ -> error "Invalid schedule"

-- | Run the target computation n times
nTimes :: MonadIO m => Int -> Target (IO a) -> m [a]
nTimes n t = liftIO $ replicateM n work
  where work = newStdGen >>= runTarget t
