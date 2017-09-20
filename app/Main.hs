{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Time.Clock     (NominalDiffTime, UTCTime, addUTCTime,
                                      getCurrentTime)
import           Data.Time.LocalTime (LocalTime, TimeZone, getCurrentTimeZone,
                                      utcToLocalTime)
import           GHC.Generics
import           Lib                 as L
import           Options.Generic
import           System.Process
import           System.Random       (newStdGen)

-- | A configuration type
data Session =
  -- | Execute @cmd@ within the period specified
  Between
    { startMs :: Int, endMs :: Int, cmd  :: String }
    -- | Execute @cmd@ after @startMs@
  | StartBy
    { startMs :: Int , cmd :: String }
    -- | Execute @cmd@ before @endMs@
  | EndBy
    { endMs :: Int , cmd :: String }
  deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = do
  s <- getRecord "Chaos" :: IO Session
  print s
  tz <- getCurrentTimeZone
  t <- getCurrentTime
  g <- newStdGen
  print ("Times offset from: " ++ (show $ toLocal tz t))
  let ta = mkSchedule s tz t
      sh = getSchedule ta
    --
  case sh of
    Just sh' -> do print ("Schedule: " ++ show sh')
                   print $ "Generated time " ++ (show $ genTime tz sh' g)
    _        -> print "No schedule computed"
    where printTarget (L.Scheduled sc ioa) = print sc
          printTarget (L.Immediate m)      = print "Immediate: action"

mkSchedule :: Session -> TimeZone -> UTCTime -> (Target (IO ()))
mkSchedule s tz t = L.scheduled (callCommand (cmd s)) $ toSchedule s tz t

toSchedule :: Session -> TimeZone -> UTCTime -> L.Schedule
toSchedule (Between s e _) tz t = L.Interval (toLocal tz (addUTCTime (nomTime s) t)) (toLocal tz (addUTCTime (nomTime e) t))
toSchedule (StartBy b _) tz t = L.Bounded (toLocal tz (addUTCTime (nomTime b) t)) Lower
toSchedule (EndBy e _)     tz t = L.Bounded (toLocal tz (addUTCTime (nomTime e) t)) Upper

nomTime :: Int -> NominalDiffTime
nomTime b = (realToFrac secs) :: NominalDiffTime
  where secs = b `div` 1000

toLocal :: TimeZone -> UTCTime -> LocalTime
toLocal tz t = utcToLocalTime tz t
