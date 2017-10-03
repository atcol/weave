{-# LANGUAGE DeriveFunctor #-}
module Data.Time.Schedule.Chaos
  (
    Bound (..),
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
    within
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
data Schedule =
              -- | A section of time from which to pick a random execcution time
              Period { pMs :: Int, tz :: TimeZone }
              -- | Perform something within the start and end times
              | Interval { start :: LocalTime, end :: LocalTime, tz :: TimeZone }
              -- | The schedule has passed
              | Finished
              deriving (Read, Show, Eq)

-- | The target action to be scheduled
data Target m = Target { sched :: Schedule, action :: m }
  deriving (Read, Show, Eq, Functor)

data Bound = Upper | Lower deriving (Show, Eq, Read, Ord)

-- | Construct a Target with the specified action
scheduled :: MonadIO m => m a -> Schedule -> Target (m a)
scheduled ioa s = Target s ioa

-- | Pull the result from the target
result :: MonadIO m => Target (m a) -> m a
result (Target _ a) = a

-- | Randomly pick a time compatible with the given schedule
genTime :: (MonadIO m, RandomGen g) => Schedule -> g -> m (Maybe LocalTime, g)
genTime (Period ms tz) rg = do
  now <- liftIO $ getCurrentTime
  let end = addUTCTime (realToFrac (ms `div` 1000)) now
  genTime (Interval (utcToLocalTime tz now) (utcToLocalTime tz end) tz) rg
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

-- | Generate a time within the given Timezone and times
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
  >>= (\t -> case sc of
               Interval s _ tz -> return $ getDelay tz s t
               Period ms tz -> do n <- getCurrentTime
                                  return $ getDelay tz (utcToLocalTime tz n) t)
  >>= threadDelay

getDelay :: TimeZone -> LocalTime -> Maybe LocalTime -> Int
getDelay _ s Nothing = error ("Invalid random time for schedule: " ++ show s)
getDelay tz s (Just t) = delay
    where tDiff = (round $ diff tz s t) :: Int
          delay = abs $ tDiff * 1000 * 1000

-- | Run the target computation n times
times :: Int -> Target (IO a) -> IO [a]
times n t = liftIO $ replicateM n work
  where work = newStdGen >>= runTarget t

-- | Run the target computation any amount of times in the interval @(a, b)@
within :: (Int, Int) -> Target (IO a) -> IO [a]
within i t = newStdGen >>= genWithin i t

-- | Run the target computation any amount of times in the interval @(a, b)@, using a supplied RandomGen
genWithin :: RandomGen g => (Int, Int) -> Target (IO a) -> g -> IO [a]
genWithin i t g = return (randomR i g) >>= return . fst >>= flip times t
