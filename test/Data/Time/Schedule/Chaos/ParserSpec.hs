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

instance Eq (IO a) where
  (==) _ _ = True

spec :: Spec
spec = parallel $ do
  describe "ParserSpec" $ do
    it "parse every" $ do

      case parseTargets "every 5 seconds { touch ./lol }" of
        Left e -> error $ "Failed with " ++ show e
        Right r -> do
          r `shouldNotBe` []
          case (head r) of
            (Offset 5000, _) -> print ""
            a                -> error $ "Incorrect response from parse:" ++ show a

    it "parse in" $ do
      case parseTargets "in 1 seconds { date }" of
        Left e  -> error $ "Failed with " ++ show e
        Right r -> r `shouldBe` [(Offset 1000, print "lol")]
