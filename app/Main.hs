{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.MVar  (MVar, newMVar)
import           Data.Maybe               (fromMaybe)
import           Data.Time.Clock          (NominalDiffTime, UTCTime, addUTCTime,
                                           getCurrentTime)
import           Data.Time.Schedule.Chaos as C
import           GHC.Generics
import           Options.Generic
import           System.Process
import           System.Random            (newStdGen)

-- | A configuration type
data Session =
  -- | Execute @cmd@ randomly between now and now + @m@s, the sionspecified number of times
  Within { ms :: Int, repeat :: Maybe Int, cmd :: String }
  -- | Execute @cmd@ any number of times between 0 and @maxTimes@
  | Randomly { ms :: Int, maxTimes :: Int, cmd :: String }
  -- | Execute @cmd@ within the period specified
  | Between { startMs :: Maybe Int, endMs :: Int, cmd :: String }
  deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = do
  s <- getRecord "Chaos" :: IO Session
  now <- getCurrentTime
  g <- newStdGen
  l <- run s $ mkTarget s now
  print l

run :: Session -> C.Target (IO a) -> IO [a]
run (Within _ (Just n) _) t = C.times n t
run (Within _ Nothing _) t  = C.times 1 t
run (Randomly _ ma _) t     = C.interval ma t
run _ t                     = C.times 1 t

mkTarget :: Session -> UTCTime -> (Target (IO ()))
mkTarget s t = C.scheduled (callCommand (cmd s)) $ toSchedule s t

toSchedule :: Session -> UTCTime -> C.Schedule
toSchedule (Within ms _ _) _ = C.Period ms
toSchedule (Randomly ms _ _) _ = C.Period ms
toSchedule (Between s e _) t = C.Interval (addUTCTime (nomTime (fromMaybe 0 s)) t) (addUTCTime (nomTime e) t)

nomTime :: Int -> NominalDiffTime
nomTime b = realToFrac secs
  where secs = b `div` 1000
