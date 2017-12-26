{-# LANGUAGE DeriveFunctor #-}
-- | The core Chaos API
module Data.Time.Schedule.Chaos
  (
    Schedule (..),

    -- | Functions
    asyncTimesIn,
    genTime,
    genWindow,
    timesIn,
    times,
    randomSeconds,
    randomTimeBetween,
    runTarget
    ) where

import           Control.Concurrent       (forkIO, takeMVar, threadDelay)
import           Control.Concurrent.Async (Async (..), async)
import           Control.Monad            (liftM, replicateM, replicateM_)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Bifunctor           (first)
import           Data.Time.Clock          (NominalDiffTime, UTCTime, addUTCTime,
                                           diffUTCTime, getCurrentTime)
import           System.Random            (Random (..), RandomGen, StdGen,
                                           newStdGen, randomR)

-- | The scheduling type, representing when an action should occur, and within which bounds
data Schedule =
              -- | A offset which to start picking a random execution time
              Offset { pMs :: Int }
              -- | Perform something within the start and end times
              | Window { start :: UTCTime, end :: UTCTime }
              -- | The schedule has passed
              | Finished
              deriving (Read, Show, Eq)

-- | The environment type, representing an event or source of events for action
-- | invocation and context
class Environment a where
  runNow :: a -> IO b

  runOn :: Schedule -> a -> IO b

-- | Randomly pick a time compatible with the given schedule
genTime :: (MonadIO m, RandomGen g) => Schedule -> g -> m (UTCTime, g)
genTime (Offset ms) rg = do
  now <- liftIO $ getCurrentTime
  let end = addUTCTime (realToFrac (ms `div` 1000)) now
  genTime (Window now end) rg
genTime sc@(Window s e) rg = return $ randomTimeBetween s e rg

-- | Shorthand for the difference between two UTCTime instances
diff :: UTCTime -> UTCTime -> NominalDiffTime
diff st en = diffUTCTime st en

-- | Generate seconds in the interval [0, n]
randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg max = first realToFrac $ randomR (0, max) rg

-- | Pick a time within the given boundaries.
randomTimeBetween :: RandomGen g => UTCTime -> UTCTime -> g -> (UTCTime, g)
randomTimeBetween s e rg = case secs of (t, ng) -> (addUTCTime t s, ng)
  where secs = randomSeconds rg (abs $ floor (diff s e))

-- | Delay for a random amount within the schedule
delayFor :: RandomGen g => Schedule -> g -> IO ()
delayFor sc g = genTime sc g
  >>= return . fst
  >>= (\ti -> case sc of
                Window s _ -> return $ getDelay s ti
                Offset ms  -> getCurrentTime >>= return . flip getDelay ti)
  >>= threadDelay

getDelay :: UTCTime -> UTCTime -> Int
getDelay s t = delay
  where tDiff = (round $ diff s t) :: Int
        micros = 1000 * 1000
        delay = abs $ tDiff * micros

-- | Randomly execute the given action within its schedule boundary
runTarget :: RandomGen g => Schedule -> IO a -> g -> IO a
runTarget sc a g = delayFor sc g >> a

-- | Schedule the action @n@ times
times :: Int -> Schedule -> IO a -> IO [a]
times n sch a = liftIO $ replicateM n work
  where work = newStdGen >>= runTarget sch a

invalidSched :: Schedule -> UTCTime -> Bool
invalidSched (Window s e) now = unsafeSchedule s e now
invalidSched _ _              = False

unsafeSchedule :: UTCTime -> UTCTime -> UTCTime -> Bool
unsafeSchedule st et now = st > et

-- | Run the action any amount of times in the interval @[1, i]@, using a supplied RandomGen
genWindow :: RandomGen g => Int -> Schedule -> IO a -> g -> IO [a]
genWindow i s a g = return (randomR (1, i) g) >>= return . fst >>= (\n -> times n s a) -- heh

-- | Run the action any amount of times in the interval @[1, a]@
timesIn :: Int -> Schedule -> IO a -> IO [a]
timesIn n s a = newStdGen >>= genWindow n s a

-- | Asynchronous convenience wrapper for @timesIn@
asyncTimesIn :: Int -> Schedule -> IO a -> IO [Async a]
asyncTimesIn n s a = timesIn n s (async a)
