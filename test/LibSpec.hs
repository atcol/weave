module LibSpec ( spec ) where

import           Data.Time.Clock           (NominalDiffTime, addUTCTime,
                                            getCurrentTime)
import           Data.Time.LocalTime       (LocalTime, getCurrentTimeZone,
                                            utcToLocalTime)
import           Lib                       (Bound (..), Schedule (..), genTime,
                                            randomSeconds, randomTimeBetween)
import           System.Random             (RandomGen, newStdGen)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Test.QuickCheck.IO        ()
import           Test.QuickCheck.Random

instance Arbitrary Schedule where
  arbitrary = interval
    where
          interval = do
                  randSt <- arbitrary
                  randEnd <- arbitrary
                  rtz <- arbitrary
                  return $ Interval randSt randEnd rtz

mx :: Int
mx = 10000

spec :: Spec
spec = do
  describe "genTime" $ do
    context "randomSeconds" $
      it "Always produces times within range" $ property prop_validRange

    context "genTime" $
      it "Produces times compatible with the given schedule" $ property prop_ValidLocalTime_WhenAfterNow

    context "randomTimeBetween" $
      it "Produces times in between the given range" $ property prop_randomTimeBetween_InRange

prop_randomTimeBetween_InRange st en = do
  g <- newStdGen
  tz <- getCurrentTimeZone
  randomTimeBetween tz st en g `shouldSatisfy` validRandomTime st en

validRandomTime :: RandomGen g => LocalTime -> LocalTime -> Maybe (LocalTime, g) -> Bool
validRandomTime st en (Just (lt, _)) = ((st <= lt) && (lt <= en)) || (st == lt) && (en == lt)
validRandomTime st en Nothing        = (st > en)

prop_ValidLocalTime_WhenAfterNow s = do
  g <- newStdGen
  mlt <- genTime s g
  mlt `shouldSatisfy` validInterval s . fst

validInterval :: Schedule -> Maybe LocalTime -> Bool
validInterval (Interval st end _) (Just lt) = (lt >= st) && (lt <= end)
validInterval (Interval st end _) Nothing   = st > end

prop_validRange v = (v > 0) ==> do
  g <- newStdGen
  randomSeconds g v `shouldSatisfy` validRange . fst

validRange :: NominalDiffTime -> Bool
validRange v = (v >= 0) && (v <= (realToFrac mx))
