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
  now <- getCurrentTime
  g <- newStdGen
  print ("Times offset from: " ++ (show $ toLocal tz now))
  let ta = mkTarget s tz now
      sh = getSchedule ta
  case sh of
    Just sh' -> do print ("Schedule: " ++ show sh')
                   gen <- genTime sh' g
                   print $ "Generated time " ++ (show gen)
    _        -> print "No schedule computed"
    where printTarget (L.Target sc ioa) = print sc

mkTarget :: Session -> TimeZone -> UTCTime -> (Target (IO ()))
mkTarget s tz t = L.scheduled (callCommand (cmd s)) $ toSchedule s tz t

toSchedule :: Session -> TimeZone -> UTCTime -> L.Schedule
toSchedule (Between s e _) tz t = L.Interval (toLocal tz (addUTCTime (nomTime s) t)) (toLocal tz (addUTCTime (nomTime e) t))

nomTime :: Int -> NominalDiffTime
nomTime b = realToFrac secs
  where secs = b `div` 1000

toLocal :: TimeZone -> UTCTime -> LocalTime
toLocal tz t = utcToLocalTime tz t
