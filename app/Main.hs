{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.MVar  (MVar, newMVar)
import           Data.Maybe               (fromMaybe)
import           Data.Time.Clock          (NominalDiffTime, UTCTime, addUTCTime,
                                           getCurrentTime)
import           Data.Time.LocalTime      (LocalTime, TimeZone,
                                           getCurrentTimeZone, utcToLocalTime)
import           Data.Time.Schedule.Chaos as C
import           GHC.Generics
import           Options.Generic
import           System.Process
import           System.Random            (newStdGen)

-- | A configuration type
data Session =
  -- | Execute @cmd@ within the period specified
  Between
    { startMs :: Maybe Int, endMs :: Int, cmd :: String, times :: Int, strategy :: Maybe String }
  deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = do
  s <- getRecord "Chaos" :: IO Session
  tz <- getCurrentTimeZone
  now <- getCurrentTime
  g <- newStdGen
  nTimes (times s) $ mkTarget s tz now
  return ()
    where printTarget (C.Target sc ioa) = print sc

mkTarget :: Session -> TimeZone -> UTCTime -> (Target (IO ()))
mkTarget s tz t = C.scheduled (callCommand (cmd s)) $ toSchedule s tz t

toSchedule :: Session -> TimeZone -> UTCTime -> C.Schedule
toSchedule (Between s e _ _ _) tz t = C.Interval (toLocal tz (addUTCTime (nomTime (fromMaybe 0 s)) t)) (toLocal tz (addUTCTime (nomTime e) t)) tz

nomTime :: Int -> NominalDiffTime
nomTime b = realToFrac secs
  where secs = b `div` 1000

toLocal :: TimeZone -> UTCTime -> LocalTime
toLocal tz t = utcToLocalTime tz t
