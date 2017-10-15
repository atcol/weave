{-# LANGUAGE DeriveFunctor #-}
-- | The core Chaos API
module Data.Time.Schedule.Chaos
  (
    Schedule (..),
    ScheduleException (..),
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
import           System.Random          (Random (..), RandomGen, StdGen,
                                         newStdGen, randomR)

import Control.Exception (throw, Exception)

data ScheduleException = InvalidScheduleException deriving (Show, Eq)

instance Exception ScheduleException

-- | The scheduling type, representing when an action should occur, and within which bounds
data Schedule =
              -- | A section of time from which to pick a random execcution time
              Period { pMs :: Int }
              -- | Perform something within the start and end times
              -- FIXME restrict this construct  so start is always <= end
              | Interval { start :: UTCTime, end :: UTCTime }
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

-- | Randomly pick a time compatible with the given schedule
genTime :: (MonadIO m, RandomGen g) => Schedule -> g -> m (UTCTime, g)
genTime (Period ms) rg = do
  now <- liftIO $ getCurrentTime
  let end = addUTCTime (realToFrac (ms `div` 1000)) now
  genTime (Interval now end) rg
genTime sc@(Interval s e) rg = return $ randomTimeBetween s e rg
  --if (invalidSched sc now) then error ("Invalid schedule " ++ show sc)
                           --else return $ randomTimeBetween s e rg

-- | Shorthand for the difference between two UTCTime instances
diff :: UTCTime -> UTCTime -> NominalDiffTime
diff st en = diffUTCTime st en

-- | Generate seconds in the interval [0, n]
randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg max = first realToFrac $ randomR (0, max) rg

-- | Generate a time within the given times.
randomTimeBetween :: RandomGen g => UTCTime -> UTCTime -> g -> (UTCTime, g)
randomTimeBetween s e rg = case secs of (t, ng) -> (addUTCTime t s, ng)
  -- if (s < e) then case secs of (t, ng) -> (addUTCTime t s, ng)
  --            else throw InvalidScheduleException
  where secs = randomSeconds rg (abs $ floor (diff s e))

-- | Randomly execute the given target within its schedule boundary
runTarget :: RandomGen g => Target (IO a) -> g -> IO a
runTarget (Target sc a) g = delayFor sc g >> a

-- | Delay for a random amount within the schedule
delayFor :: RandomGen g => Schedule -> g -> IO ()
delayFor sc g = genTime sc g
  >>= return . fst
  >>= (\ti -> case sc of
                Interval s _ -> return $ getDelay s ti
                Period ms    -> getCurrentTime >>= return . flip getDelay ti)
  >>= threadDelay

getDelay :: UTCTime -> UTCTime -> Int
getDelay s t = delay
  where tDiff = (round $ diff s t) :: Int
        micros = 1000 * 1000
        delay = abs $ tDiff * micros

-- | Run the target computation n times
times :: Int -> Target (IO a) -> IO [a]
times n t = liftIO $ replicateM n work
  where work = newStdGen >>= runTarget t

-- | Run the target computation any amount of times in the interval @[1, a]@
within :: Int -> Target (IO a) -> IO [a]
within n tar@(Target sch _) = newStdGen >>= genWithin n tar

invalidSched :: Schedule -> UTCTime -> Bool
invalidSched (Interval s e) now = unsafeSchedule s e now
invalidSched _ _                = False

unsafeSchedule :: UTCTime -> UTCTime -> UTCTime -> Bool
unsafeSchedule st et now = (st > et)

-- | Run the target computation any amount of times in the interval @[1, a]@, using a supplied RandomGen
genWithin :: RandomGen g => Int -> Target (IO a) -> g -> IO [a]
genWithin i t g = return (randomR (1, i) g) >>= return . fst >>= flip times t
