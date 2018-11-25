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
  Parse String
  | From String
  deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = do
  s <- getRecord "Chaos" :: IO Session
  now <- getCurrentTime
  g <- newStdGen
  case s of
    From f  -> B.readFile f >>= return . CP.parsePlan >>= handleParse >> return ()
    Parse s -> return (B.pack s) >>= return . CP.parsePlan >>= handleParse >> return ()

handleParse = either error C.runPlan
