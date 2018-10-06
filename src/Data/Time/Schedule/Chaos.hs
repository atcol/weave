{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The core Chaos API
module Data.Time.Schedule.Chaos
  (
    Schedule (..),

    -- | Functions
    asyncTimesIn,
    genTime,
    genWindow,
    mkOffsets,
    mkSchedules,
    timesIn,
    times,
    randomSeconds,
    randomTimeBetween,
    runSchedule,
    runSchedules,
    mergeAndRunSchedules,
    runTarget
    ) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async (..), async)
import           Control.Monad            (replicateM)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (Reader, runReader)
import           Data.Bifunctor           (first)
import           Data.List                (repeat)
import           Data.Time.Clock          (NominalDiffTime, UTCTime, addUTCTime,
                                           diffUTCTime, getCurrentTime)
import           GHC.Generics
import           System.Random            (Random (..), RandomGen, newStdGen,
                                           randomR)

-- | The scheduling type, representing when an action should occur and its bounds
data Schedule =
  -- | An point in the future
  Offset { pMs :: Int }
  -- | Perform something within the start and end times
  | Window { start :: UTCTime, end :: UTCTime }
  deriving (Read, Show, Eq, Generic)

-- | Asynchronous convenience wrapper for @timesIn@
asyncTimesIn :: Int -> Schedule -> IO a -> IO [Async a]
asyncTimesIn n s a = timesIn n s (async a)

-- | Randomly pick a time compatible with the given schedule
genTime :: (MonadIO m, RandomGen g) => Schedule -> g -> m (UTCTime, g)
genTime (Offset ms) rg = do
  now <- liftIO $ getCurrentTime
  let end' = addUTCTime (realToFrac (ms `div` 1000)) now
  genTime (Window now end') rg
genTime (Window s e) rg = return $ randomTimeBetween s e rg

-- | Shorthand for the difference between two UTCTime instances
diff :: UTCTime -> UTCTime -> NominalDiffTime
diff st en = diffUTCTime st en

-- | Delay for a random amount within the schedule
delayFor :: RandomGen g => Schedule -> g -> IO g
delayFor sc g = do
    (ti, g') <- genTime sc g
    del <- case sc of
                Window s _ -> return $ getDelay s ti
                Offset _   -> getCurrentTime >>= return . flip getDelay ti
    threadDelay del
    return g'

-- | Delay exactly as the schedule suggests, then run the action
delayRun :: Schedule -> IO a -> IO a
delayRun (Offset ms) a = threadDelay (ms * 1000) >> a
delayRun s _           = error $ "Not yet supported: " ++ show s


-- | Turn the @UTCTime@ to its microseconds
getDelay :: UTCTime -> UTCTime -> Int
getDelay s t = delay
  where tDiff = (round $ diff s t) :: Int
        micros = 1000 * 1000
        delay = abs $ tDiff * micros

-- | Run the action any amount of times in the interval @[1, i]@, using a supplied RandomGen
genWindow :: RandomGen g => Int -> Schedule -> IO a -> g -> IO [a]
genWindow i s a g = return (randomR (1, i) g) >>= return . fst >>= (\n -> times n s a) -- heh

-- | Construct an infinite list of constant @Offset@ instances
mkOffsets :: Int -> [Schedule]
mkOffsets n = fmap Offset $ repeat n

-- | Construct the schedule using the supplied reader
mkSchedules :: Reader e Schedule -> e -> [IO a] -> [(Schedule, IO a)]
mkSchedules r e acts = map (\ac -> (runReader r e, ac)) acts

-- | Generate seconds in the interval [0, n]
randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg mx = first realToFrac $ randomR (0, mx) rg

-- | Pick a time within the given boundaries.
randomTimeBetween :: RandomGen g => UTCTime -> UTCTime -> g -> (UTCTime, g)
randomTimeBetween s e rg = case secs of (t, ng) -> (addUTCTime t s, ng)
  where secs = randomSeconds rg (abs $ floor (diff s e))

-- | Randomly execute the given action within its schedule boundary
runTarget :: RandomGen g => Schedule -> IO a -> g -> IO a
runTarget sc a g = delayFor sc g >> a

runSchedule :: (Schedule, IO a) -> IO a
runSchedule (sc, a) = delayRun sc a

-- | Run the specified action-schedule pairs
runSchedules :: [(Schedule, IO a)] -> IO [a]
runSchedules scs = mapM runSchedule scs

-- | Combine the pairs from each list & run them. Uneven lists will yield @[]@
mergeAndRunSchedules :: [Schedule] -> [IO a] -> IO [a]
mergeAndRunSchedules x y = runSchedules $ toPairs x y
  where toPairs (x':xs) (y':ys) = (x', y') : toPairs xs ys
        toPairs [] _            = []
        toPairs _ []            = []

-- | Schedule the action @n@ times
times :: Int -> Schedule -> IO a -> IO [a]
times n sch a = liftIO $ replicateM n work
  where work = newStdGen >>= runTarget sch a

-- | Run the action any amount of times in the interval @[1, n]@
timesIn :: Int -> Schedule -> IO a -> IO [a]
timesIn n s a = newStdGen >>= genWindow n s a
