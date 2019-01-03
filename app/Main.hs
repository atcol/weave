{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Either     (either)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           GHC.Generics
import           Options.Generic
import           Prelude         (error)
import           Protolude
import           System.Process
import           Weave           as W
import           Weave.Parser    as WP

-- | A configuration type
data Session = Session { filename :: Maybe FilePath , raw :: Maybe T.Text }
  deriving (Show, Generic)

instance ParseRecord Session

main :: IO ()
main = (getRecord "Chaos" :: IO Session) >>= execute

execute (Session (Just f) _) = T.readFile f >>= return . WP.parsePlan >>= handleParse >> return ()
execute (Session _ (Just s)) = return s >>= return . WP.parsePlan >>= handleParse >> return ()
execute  _                   = getContents >>= return . Session Nothing . Just >>= execute

handleParse (WP.Success p)    = W.runPlan p
handleParse (MalformedPlan e) = T.putStrLn e
