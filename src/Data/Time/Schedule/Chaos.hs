{-# LANGUAGE DeriveFunctor #-}
-- | The core Chaos API
module Data.Time.Schedule.Chaos
  (
    Schedule (..),
    Target (..),

    -- | Functions
    genTime,
    genWithin,
    times,
    randomSeconds,
    randomTimeBetween,
    result,
    runTarget,
    scheduled,
    within,
    unsafeSchedule
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

data ScheduleException = InvalidScheduleException deriving (Show, Eq)

-- | The scheduling type, representing when an action should occur, and within which bounds
data Schedule =
              -- | A section of time from which to pick a random execcution time
              Period { pMs :: Int, tz :: TimeZone }
              -- | Perform something within the start and end times
              -- FIXME restrict this construct  so start is always <= end
              | Interval { start :: LocalTime, end :: LocalTime, tz :: TimeZone }
              -- | The schedule has passed
              | Finished
              deriving (Read, Show, Eq)

-- | The target action to be scheduled
data Target m = Target { sched :: Schedule, action :: m }
  deriving (Read, Show, Eq, Functor)

-- | Construct a Target with the specified action
scheduled :: MonadIO m => m a -> Schedule -> Target (m a)
scheduled ioa s = Target s ioa

-- | Pull the result from the target
result :: MonadIO m => Target (m a) -> m a
result (Target _ a) = a

-- | Randomly pick a time compatible with the given schedule. Start must be <= end
genTime :: (MonadIO m, RandomGen g) => Schedule -> g -> m (Maybe LocalTime, g)
genTime (Period ms tz) rg = do
  now <- liftIO $ getCurrentTime
  let end = addUTCTime (realToFrac (ms `div` 1000)) now
  genTime (Interval (utcToLocalTime tz now) (utcToLocalTime tz end) tz) rg
genTime (Interval s e t) rg = do
  return $ mapRes $ randomTimeBetween t s e rg
  --if (s <= e) then return $ mapRes $ randomTimeBetween t s e rg
              --else return (Nothing, rg)
  where mapRes (Just (a, b)) = (Just a, b)
        mapRes Nothing       = (Nothing, rg)

-- | Provide the difference between two LocalTime instances
diff :: TimeZone -> LocalTime -> LocalTime -> NominalDiffTime
diff tz st en = diffUTCTime (localTimeToUTC tz st) (localTimeToUTC tz en)

-- | Generate seconds in the interval [0, n]
randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg max = first realToFrac $ randomR (0, max) rg

-- | Generate a time within the given Timezone and times. Start time must be <= end.
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
delayFor sc g =
  genTime sc g
  >>= return . fst
  >>= (\t -> case t of
               Nothing -> error ("Invalid schedule: " ++ show sc)
               (Just ti) -> case sc of
                              Interval s _ tz -> return $ getDelay tz s ti
                              Period ms tz -> do n <- getCurrentTime
                                                 return $ getDelay tz (utcToLocalTime tz n) ti)
  >>= threadDelay

getDelay :: TimeZone -> LocalTime -> LocalTime -> Int
getDelay tz s t = delay
    where tDiff = (round $ diff tz s t) :: Int
          delay = abs $ tDiff * micros
          micros = 1000 * 1000

-- | Run the target computation n times
times :: Int -> Target (IO a) -> IO [a]
times n t = liftIO $ replicateM n work
  where work = newStdGen >>= runTarget t

-- | Run the target computation any amount of times in the interval @[0, a]@
within :: Int -> Target (IO a) -> IO [a]
within n tar@(Target sch _) = do
  now <- getCurrentTime >>= return . utcToLocalTime (tz sch)
  if (invalidSched sch now) then error ("Invalid schedule " ++ show sch)
                          else newStdGen >>= genWithin n tar
    where invalidSched (Interval s e t) now = unsafeSchedule s e now
          invalidSched _ _                  = False

unsafeSchedule :: LocalTime -> LocalTime -> LocalTime -> Bool
unsafeSchedule st et now = (st > et) || (et <= now)

-- | Run the target computation any amount of times in the interval @[0, a]@, using a supplied RandomGen
genWithin :: RandomGen g => Int -> Target (IO a) -> g -> IO [a]
genWithin i t g = return (randomR (0, i) g) >>= return . fst >>= flip times t
