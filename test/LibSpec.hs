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

prop_ValidLocalTime_WhenAfterNow s = do
  g <- newStdGen
  mlt <- genTime s g
  mlt `shouldSatisfy` validInterval s

validInterval :: Schedule -> Either String LocalTime -> Bool
validInterval (Interval st end) (Right lt) = (lt >= st) && (lt <= end)
validInterval (Interval st end) (Left err) = err == "Start is > end"
validInterval (Bounded t Upper) (Right lt) = lt < t
validInterval (Bounded t Lower) (Right lt) = lt > t
validInterval _ _                          = False

prop_validRange v = (v > 0) ==> do
  g <- newStdGen
  randomSeconds g v `shouldSatisfy` validRange . fst

validRange :: NominalDiffTime -> Bool
validRange v = (v >= 0) && (v <= (realToFrac mx))
