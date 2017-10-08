{-# LANGUAGE FlexibleInstances #-}
module Data.Time.Schedule.ChaosSpec ( spec ) where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Time.Clock           (NominalDiffTime, UTCTime,
                                            addUTCTime, getCurrentTime)
import           Data.Time.LocalTime       (LocalTime, getCurrentTimeZone,
                                            utcToLocalTime)
import           Data.Time.Schedule.Chaos  (Schedule (..), Target (..), genTime,
                                            randomSeconds, randomTimeBetween,
                                            within)
import           Debug.Trace               (traceShow)
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
    where period = Period <$> arbitrary <*> arbitrary
          interval = do
                  randSt <- arbitrary
                  randEnd <- arbitrary
                  rtz <- arbitrary
                  return $ Interval randSt randEnd rtz

instance Arbitrary (Target (IO String)) where
  arbitrary = do
    s <- arbitrary :: Gen String
    sc <- arbitrary
    return $ Target sc (return s)

instance Show (IO a) where
  show _ = "IO a"

mx :: Int
mx = 10000

spec :: Spec
spec = do
  describe "Randomisation & execution" $ do
    context "randomSeconds" $
      prop "Always produces times within range" $ prop_validRange

    context "genTime" $
      prop "Produces times compatible with the given schedule" $ prop_ValidLocalTime_WhenAfterNow

    context "randomTimeBetween" $
      prop "Produces times in between the given range" $ prop_randomTimeBetween_InRange

    context "within" $
      prop "Runs number of times within the interval specified" $ prop_within_alwaysInRange

prop_within_alwaysInRange i@(s, e) t = ((s <= e) && (s >= 0) && (e < 6000)) ==> do
  traceShow i $ within i (t :: Target (IO String)) `shouldNotReturn` (return "")

prop_randomTimeBetween_InRange st en = do
  g <- newStdGen
  tz <- getCurrentTimeZone
  randomTimeBetween tz st en g `shouldSatisfy` validRandomTime st en

validRandomTime :: RandomGen g => LocalTime -> LocalTime -> Maybe (LocalTime, g) -> Bool
validRandomTime st en (Just (lt, _)) = ((st <= lt) && (lt <= en)) || (st == lt) && (en == lt)
validRandomTime st en Nothing        = (st > en)

prop_ValidLocalTime_WhenAfterNow s = do
  now <- getCurrentTime
  g <- newStdGen
  mlt <- genTime s g
  mlt `shouldSatisfy` validInterval now s . fst

validInterval :: UTCTime -> Schedule -> Maybe LocalTime -> Bool
validInterval _ (Interval st end _) (Just lt) = (lt >= st) && (lt <= end)
validInterval _ (Interval st end _) Nothing   = st > end
validInterval now (Period n tz) (Just lt)     = n <= 0 || lt >= (utcToLocalTime tz now)
validInterval _ (Period n _) Nothing          = n < 0

prop_validRange v = (v > 0) ==> do
  g <- newStdGen
  randomSeconds g v `shouldSatisfy` validRange . fst

validRange :: NominalDiffTime -> Bool
validRange v = (v >= 0) && (v <= (realToFrac mx))
