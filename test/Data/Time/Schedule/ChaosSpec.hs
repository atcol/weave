{-# LANGUAGE FlexibleInstances #-}
module Data.Time.Schedule.ChaosSpec ( spec ) where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad.IO.Class    (liftIO)
import           Data.Time.Clock           (NominalDiffTime, UTCTime,
                                            addUTCTime, getCurrentTime)
import           Data.Time.Schedule.Chaos  (Schedule (..), Target (..), genTime,
                                            randomSeconds, randomTimeBetween,
                                            unsafeSchedule, within, ScheduleException)
import           Debug.Trace               (traceM, traceShow)
import           System.IO.Unsafe          (unsafePerformIO)
import           System.Random             (RandomGen, newStdGen)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           hiding (within)
import           Test.QuickCheck.Instances
import           Test.QuickCheck.IO        ()
import           Test.QuickCheck.Random

instance Arbitrary Schedule where
  arbitrary = oneof [interval, period]
    where period = Period <$> arbitrary
          interval = Interval <$> arbitrary <*> arbitrary

instance Arbitrary (Target (IO String)) where
  arbitrary = do
    sc <- arbitrary
    return $ Target sc (return "Test IO action")

instance Show (IO a) where
  show _ = "IO a"

mx :: Int
mx = 10000

spec :: Spec
spec = do
  describe "randomSeconds" $
    prop "Always produces times within range" $ prop_validRange

  describe "genTime" $
    prop "Produces times compatible with the given schedule" $ prop_ValidTime_WhenAfterNow

  describe "randomTimeBetween" $
    prop "Produces times in between the given range" $ prop_randomTimeBetween_InRange

  describe "within" $ do
    now <- runIO $ getCurrentTime
    prop "Runs number of times within a *valid* interval" $ prop_within_alwaysInRange now

prop_within_alwaysInRange n e t@(Target (Interval st en) _) =
  intervalRestriction n e t  ==> do
    traceShow n (putStrLn $ show t)
    within e (t :: Target (IO String)) `shouldNotReturn` (return [])
prop_within_alwaysInRange n e t@(Target (Period ms) _) =
  (e >= 0) && (e < 6000) && (ms >= 0) ==> do
    traceShow n (putStrLn $ show t)
    let val = within e (t :: Target (IO String))
    val `shouldNotReturn` (return [])

intervalRestriction n e t@(Target (Interval st en) _) =
  ((n <= st) && (n <= en) && (e < 6000))

prop_randomTimeBetween_InRange st en = do
  g <- newStdGen
  randomTimeBetween st en g `shouldSatisfy` validRandomTime st en

  --if (st < en) then randomTimeBetween st en g `shouldSatisfy` validRandomTime st en
               --else randomTimeBetween st en g `shouldThrow` Selector InvalidScheduleException

validRandomTime :: RandomGen g => UTCTime -> UTCTime -> (UTCTime, g) -> Bool
validRandomTime st en (nt, _) = if (st <= en) then (nt >= st) && (nt <= en)
                                              else (nt <= st) || (nt >= en)

prop_ValidTime_WhenAfterNow s = do
  now <- getCurrentTime
  g <- newStdGen
  mlt <- genTime s g
  mlt `shouldSatisfy` validInterval now s . fst

validInterval :: UTCTime -> Schedule -> UTCTime -> Bool
validInterval _ (Interval st end) nt = if (st < end) then (nt >= st) && (nt <= end)
                                                     else (nt <= st) || (nt >= end) -- reverse of above

validInterval now (Period n) nt     = n <= 0 || nt >= now

prop_validRange v = (v > 0) ==> do
  g <- newStdGen
  randomSeconds g v `shouldSatisfy` validRange . fst

validRange :: NominalDiffTime -> Bool
validRange v = (v >= 0) && (v <= (realToFrac mx))
