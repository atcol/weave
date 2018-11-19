{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Data.ByteString.Char8           as B
import           Data.Either                     (either)
import           Data.Maybe                      (fromMaybe)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime,
                                                  addUTCTime, getCurrentTime)
import           Data.Time.Schedule.Chaos        as C
import           Data.Time.Schedule.Chaos.Parser as CP
import           GHC.Generics
import           Options.Generic
import           System.Process
import           System.Random                   (newStdGen)

-- | A configuration type
data Session =
  -- | Execute @cmd@ randomly between now and now + @ms@, the specified number of times
  Within { ms :: Int, repeat :: Maybe Int, cmd :: String }
  -- | Execute @cmd@ any number of times between 0 and @maxTimes@
  | Randomly { ms :: Int, maxTimes :: Int, cmd :: String }
  -- | Execute @cmd@ within the period specified
  | Between { startMs :: Maybe Int, endMs :: Int, cmd :: String }
  | From { filename :: String }
  deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = do
  s <- getRecord "Chaos" :: IO Session
  now <- getCurrentTime
  g <- newStdGen
  case s of
    From f -> B.readFile f >>= return . CP.parseTargets >>= handleParse
    _      -> run s (toSchedule s now) (callCommand (cmd s)) >> return ()

handleParse = either error C.runSchedule

run :: Session -> Schedule -> (IO a) -> IO [a]
run (Within _ (Just n) _) t = C.times n t
run (Within _ Nothing _) t  = C.times 1 t
run (Randomly _ ma _) t     = C.times ma t
run _ t                     = C.times 1 t

toSchedule :: Session -> UTCTime -> C.Schedule
toSchedule (Within ms _ _) _   = C.Offset ms
toSchedule (Randomly ms _ _) _ = C.Offset ms
toSchedule (Between s e _) t   = undefined -- C.Window (addUTCTime (nomTime (fromMaybe 0 s)) t) (addUTCTime (nomTime e) t)

nomTime :: Int -> NominalDiffTime
nomTime b = realToFrac secs
  where secs = b `div` 1000
