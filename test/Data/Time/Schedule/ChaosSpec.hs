{-# LANGUAGE FlexibleInstances #-}
module Data.Time.Schedule.ChaosSpec ( spec ) where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (Reader)
import           Data.Time.Clock           (NominalDiffTime, UTCTime,
                                            addUTCTime, getCurrentTime)
import           Data.Time.Schedule.Chaos  (Schedule (..), genTime, mkSchedules,
                                            randomSeconds, randomTimeBetween,
                                            timesIn)
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
    where period = Offset <$> arbitrary
          interval = Window <$> arbitrary <*> arbitrary

instance Arbitrary (IO String) where
  arbitrary = return (return "Test IO action")

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

  describe "interval" $ do
    now <- runIO $ getCurrentTime
    prop "Runs number of times within a *valid* interval" $ prop_interval_alwaysInRange now

  describe "mkSchedules" $ do
    prop "Benign on empty input" $ prop_mkSchedule_benign_empty_input

prop_mkSchedule_benign_empty_input sc = do
  length (mkSchedules (return sc) () []) `shouldBe` 0

prop_interval_alwaysInRange n e sc@(Window st en) ioa =
  intervalRestriction n e sc ==> do
    --traceShow n (putStrLn $ show sc)
    timesIn e sc (ioa :: IO String) `shouldNotReturn` (return [])
prop_interval_alwaysInRange n e sc@(Offset ms) ioa =
  (e >= 0) && (e < 6000) && (ms >= 0) ==> do
    --traceShow n (putStrLn $ show sc)
    let val = timesIn e sc (ioa :: IO String)
    val `shouldNotReturn` (return [])

intervalRestriction n e sc@(Window st en) = ((n <= st) && (n <= en) && (e < 6000))

prop_randomTimeBetween_InRange st en = do
  g <- newStdGen
  randomTimeBetween st en g `shouldSatisfy` validRandomTime st en

validRandomTime :: RandomGen g => UTCTime -> UTCTime -> (UTCTime, g) -> Bool
validRandomTime st en (nt, _) = if (st <= en) then (nt >= st) && (nt <= en)
                                              else (nt <= st) || (nt >= en)

prop_ValidTime_WhenAfterNow s = do
  now <- getCurrentTime
  g <- newStdGen
  mlt <- genTime s g
  mlt `shouldSatisfy` validSchedule now s . fst

validSchedule :: UTCTime -> Schedule -> UTCTime -> Bool
validSchedule _ (Window st end) nt = if (st < end) then (nt >= st) && (nt <= end)
                                                     else (nt <= st) || (nt >= end) -- reverse of above

validSchedule now (Offset n) nt     = n <= 0 || nt >= now

prop_validRange v = (v > 0) ==> do
  g <- newStdGen
  randomSeconds g v `shouldSatisfy` validRange . fst

validRange :: NominalDiffTime -> Bool
validRange v = (v >= 0) && (v <= (realToFrac mx))
