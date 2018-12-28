{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Either     (either)
import           Data.Maybe      (fromMaybe)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime,
                                  getCurrentTime)
import           GHC.Generics
import           Options.Generic
import           System.Process
import           System.Random   (newStdGen)
import           Weave           as W
import           Weave.Parser    as WP

-- | A configuration type
data Session = Session { filename :: Maybe String , raw :: Maybe String }
  deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = (getRecord "Chaos" :: IO Session) >>= execute

execute (Session (Just f) _) = T.readFile f >>= return . WP.parsePlan >>= handleParse >> return ()
execute (Session _ (Just s)) = return (T.pack s) >>= return . WP.parsePlan >>= handleParse >> return ()
execute  _                   = getContents >>= return . Session Nothing . Just >>= execute

handleParse = either error W.runPlan
