module LibSpec ( spec ) where

import           Data.Time.Clock           (NominalDiffTime, addUTCTime,
                                            getCurrentTime)
import           Data.Time.LocalTime       (LocalTime, getCurrentTimeZone,
                                            utcToLocalTime)
import           Lib                       (Bound (..), Schedule (..), genTime,
                                            randomSeconds)
import           System.Random             (RandomGen, newStdGen)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Test.QuickCheck.IO        ()
import           Test.QuickCheck.Random

instance Arbitrary Schedule where
  arbitrary = oneof [interval, bounded]
    where
          interval = do
                  randSt <- arbitrary
                  randEnd <- arbitrary
                  return $ Interval randSt randEnd
          bounded = do
                  t <- arbitrary
                  bound <- elements [Upper, Lower]
                  return $ Bounded t bound

mx :: Int
mx = 10000

spec :: Spec
spec = do
  describe "genTime" $ do
    context "randomSeconds" $ do
      it "Always produces times within range" $ property prop_validRange

    context "genTime" $ do
      it "Produces times compatible with the given schedule" $ property prop_genTime_CreatesValidLocalTime_WhenScheduleAfterNow

prop_genTime_CreatesValidLocalTime_WhenScheduleAfterNow tz s@(Bounded t b) = do
  g <- newStdGen
  genTime tz s g `shouldSatisfy` validInterval s
prop_inSchedule_AfterNow tz s@(Interval st end) = do
  g <- newStdGen
  now <- getCurrentTime
  nowTz <- getCurrentTimeZone
  -- Must be after now for this property
  if (st <= (utcToLocalTime nowTz now)) then True `shouldBe` True
                                  else genTime tz s g `shouldSatisfy` validInterval s

validInterval :: Schedule -> Maybe LocalTime -> Bool
validInterval (Interval st end) (Just lt) = (lt >= st) && (lt <= end)
validInterval (Bounded t Upper) (Just lt) = lt < t
validInterval (Bounded t Lower) (Just lt) = lt > t
validInterval _ _                         = False

--prop_validRange :: RandomGen g => g -> Int -> (NominalDiffTime -> Bool)
prop_validRange v = (v > 0) ==> do
  g <- newStdGen
  randomSeconds g v `shouldSatisfy` validRange

validRange :: NominalDiffTime -> Bool
validRange v = (v >= 0) && (v <= (realToFrac mx))
