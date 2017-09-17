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

-- | A configuration type
data Session = StartBy
                 {
                   diffMs   :: Int
                 , withinMs :: Int
                 , cmd      :: String
                 }
              | EndBy
                 {
                   endByMs :: Int
                 , cmd     :: String
                 }
             deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = do
  s <- getRecord "Chaos" :: IO Session
  print s
  tz <- getCurrentTimeZone
  t <- getCurrentTime
  print ("Times offset from: " ++ (show $ toLocal tz t))
  printTarget $ mkSchedule s tz t
    where printTarget (L.Scheduled sc ioa) = print sc
          printTarget (L.Immediate m)      = print "Immediate: action"

mkSchedule :: Session -> TimeZone -> UTCTime -> (Target (IO ()))
mkSchedule s tz t = L.scheduled (callCommand (cmd s)) $ toSchedule s tz t

toSchedule :: Session -> TimeZone -> UTCTime -> L.Schedule
toSchedule (StartBy b w c) tz t = L.Bounded (toLocal tz (addUTCTime (nomTime b) t)) Lower (Just w)
toSchedule (EndBy e c)     tz t = L.Bounded (toLocal tz (addUTCTime (nomTime e) t)) Upper Nothing

nomTime :: Int -> NominalDiffTime
nomTime b = (realToFrac secs) :: NominalDiffTime
  where secs = b `div` 1000


toLocal :: TimeZone -> UTCTime -> LocalTime
toLocal tz t = utcToLocalTime tz t
