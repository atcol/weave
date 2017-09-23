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
      it "Produces times compatible with the given schedule" $ property prop_ValidLocalTime_WhenAfterNow

prop_ValidLocalTime_WhenAfterNow tz s@(Bounded t b) = do
  g <- newStdGen
  nowTz <- getCurrentTimeZone
  nowUtc <- getCurrentTime
  let now = (utcToLocalTime tz nowUtc)
  if (t <= now) then True `shouldBe` True
                else case b of
                      Upper -> genTime tz now t g `shouldSatisfy` validInterval s
                      Lower -> genTime tz t now g `shouldSatisfy` validInterval s
prop_ValidLocalTime_WhenAfterNow tz s@(Interval st end) = do
  g <- newStdGen
  nowTz <- getCurrentTimeZone
  nowUtc <- getCurrentTime
  let now = (utcToLocalTime tz nowUtc)
  if (st <= now) then True `shouldBe` True
                 else genTime tz st end g `shouldSatisfy` validInterval s

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
