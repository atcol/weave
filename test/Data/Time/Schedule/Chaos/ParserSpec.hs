{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Schedule.Chaos.ParserSpec ( spec ) where

import           Data.Time.Clock                 (NominalDiffTime, UTCTime,
                                                  addUTCTime, getCurrentTime)
import           Data.Time.Schedule.Chaos        (Schedule (..),
                                                  mergeAndRunSchedules,
                                                  mkOffsets, runSchedules)
import           Data.Time.Schedule.Chaos.Parser (parseTargets)
import           Debug.Trace                     (traceM, traceShow)
import           System.IO.Unsafe                (unsafePerformIO)
import           Test.Hspec

instance Show (IO a) where
  show _ = "IO a"

spec :: Spec
spec = parallel $ do
  describe "ParserSpec" $ do
    it "parse every " $ do

      case parseTargets "every 5 seconds { touch ./lol }" of
        Left e -> error $ "Failed with " ++ show e
        Right r -> do
          length r `shouldBe` 1

          case (head r) of
            (Offset 5000, _) -> print "yay"
            a                -> error $ "Incorrect response from parse:" ++ show a
