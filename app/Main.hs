{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.MVar (MVar, newMVar)
import           Data.Maybe              (fromMaybe)
import           Data.Time.Clock         (NominalDiffTime, UTCTime, addUTCTime,
                                          getCurrentTime)
import           Data.Time.LocalTime     (LocalTime, TimeZone,
                                          getCurrentTimeZone, utcToLocalTime)
import           GHC.Generics
import           Lib                     as L
import           Options.Generic
import           System.Process
import           System.Random           (newStdGen)

-- | A configuration type
data Session =
  -- | Execute @cmd@ within the period specified
  Between
    { startMs :: Maybe Int, endMs :: Int, cmd :: String }
  deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = do
  s <- getRecord "Chaos" :: IO Session
  tz <- getCurrentTimeZone
  now <- getCurrentTime
  g <- newStdGen
  let ta = mkTarget s tz now
      sh = getSchedule ta
  case sh of
    Just sh' -> runTarget ta
    _        -> print "No schedule computed"
    where printTarget (L.Target sc ioa) = print sc

mkTarget :: Session -> TimeZone -> UTCTime -> (Target (IO ()))
mkTarget s tz t = L.scheduled (callCommand (cmd s)) $ toSchedule s tz t

toSchedule :: Session -> TimeZone -> UTCTime -> L.Schedule
toSchedule (Between s e _) tz t = L.Interval (toLocal tz (addUTCTime (nomTime (fromMaybe 0 s)) t)) (toLocal tz (addUTCTime (nomTime e) t)) tz

nomTime :: Int -> NominalDiffTime
nomTime b = realToFrac secs
  where secs = b `div` 1000

toLocal :: TimeZone -> UTCTime -> LocalTime
toLocal tz t = utcToLocalTime tz t
