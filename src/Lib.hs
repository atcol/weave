{-# LANGUAGE DeriveFunctor #-}
module Lib
  (
    -- | Types
    Schedule (..),
    Target (..),
    Bound (..),

    -- | Functions
    immediately,
    genTime,
    getSchedule,
    randomSeconds,
    result,
    startTime,
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
                -- | Begin after, or by, the given time
                | Bounded { time :: LocalTime, bound :: Bound }
                | Finished
                deriving (Show, Eq)

-- | The target action to be scheduled
data Target m = Immediate { action :: m }
              | Scheduled { sched :: Schedule, action :: m }
              deriving (Show, Eq, Functor)

--instance Monoid Schedule where
--  mempty = Finished
--  mappend (Interval s1 e1) (Interval s2 e2) = Interval (s1 + s2) (e1 + e2)
--  mappend (Bounded s1 Upper) (Interval s2 e2) = Interval (s1 + s2) (e1 + e2)

-- | Retrieve the required schedule
getSchedule :: MonadIO m => Target (m a) -> Maybe Schedule
getSchedule (Immediate _)   = Nothing
getSchedule (Scheduled s _) = Just s

-- | Construct an immediate schedule
immediately :: MonadIO m => m a -> Target (m a)
immediately ioa = Immediate ioa

-- | Construct an schedule with the specified action
scheduled :: MonadIO m => m a -> Schedule -> Target (m a)
scheduled ioa s = Scheduled s ioa

-- | Pull the result from the target
result :: MonadIO m => Target (m a) -> m a
result (Immediate a)   = a
result (Scheduled _ a) = a

startTime :: Schedule -> Maybe LocalTime
startTime (Interval s _)    = Just s
startTime (Bounded s Lower) = Just s
startTime _                 = Nothing

-- | Randomly pick a time compatible with the given schedule
--genTime :: RandomGen g => TimeZone -> Schedule -> g -> Maybe LocalTime
genTime :: RandomGen g => TimeZone -> LocalTime -> LocalTime -> g -> Maybe LocalTime
genTime tz s e rg = if (s < e) && (s /= e) then (Just $ utcToLocalTime tz (randTime s e))
                                           else Nothing
  where randTime st en = addUTCTime (randStart (diff s e)) (localTimeToUTC tz st)
        diff st en  = floor (diffUTCTime (localTimeToUTC tz st) (localTimeToUTC tz en))
        randStart d = randomSeconds rg d
--  case s of
--    (Bounded s Upper) -> Just $ utcToLocalTime tz (addUTCTime (- (randStart )) (localTimeToUTC tz st))
--    (Bounded s Lower) -> Nothing
--    (Interval s e) -> Just $ utcToLocalTime tz (randTime s)

randomSeconds :: RandomGen g => g -> Int -> NominalDiffTime
randomSeconds rg max = realToFrac (fst (randomR (0, max) rg))
