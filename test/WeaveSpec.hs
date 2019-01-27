{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WeaveSpec ( spec ) where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (Reader, asks)
import qualified Data.Text                 as T
import           Data.Time.Clock           (NominalDiffTime, UTCTime (..),
                                            diffUTCTime, getCurrentTime)
import           Debug.Trace               (traceM, traceShow)
import           Protolude
import           System.IO.Unsafe          (unsafePerformIO)
import           System.Random             (RandomGen, newStdGen)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           hiding (within)
import           Test.QuickCheck.Instances
import           Test.QuickCheck.IO        ()
import           Test.QuickCheck.Random
import           Weave

instance Arbitrary Schedule where
  arbitrary = oneof [interval, period]
    where period = Offset <$> arbitrary
          interval = Window <$> arbitrary <*> arbitrary

instance Arbitrary (IO T.Text) where
  arbitrary = return (return "Fake IO action")

mx :: Int
mx = 10000

action :: IO T.Text
action = print "Hi!" >> return "Hello"

spec :: Spec
spec = do
  describe "Chaos API" $ do
    context "Demonstrate" $ do
      it "`Cause` and `Schedule` - single" $ do
        tBefore <- getCurrentTime

        -- | Generate the next event, after a 1 second delay
        next (Offset 1000) action `shouldReturn` "Hello"
        tAfter <- getCurrentTime
        diffUTCTime tAfter tBefore `shouldSatisfy` ((==) 1 . round)

      it "`Cause` and `Schedule` - times" $ do
        tBefore <- getCurrentTime

        -- | Generate 10 events, each with a 100 ms delay
        times 10 (Offset 100) action `shouldReturn` (replicate 10 "Hello")
        tAfter <- getCurrentTime
        diffUTCTime tAfter tBefore `shouldSatisfy` ((==) 1 . round)

  describe "Chaos 'Helper' Functions" $ do
    context "randomSeconds" $
      prop "Always produces times within range" $ prop_validRange

    context "genTime" $
      prop "Produces times compatible with the given schedule" $ prop_ValidTime_WhenAfterNow

    context "randomTimeBetween" $
      prop "Produces times in between the given range" $ prop_randomTimeBetween_InRange

    context "interval" $ do
      now <- runIO $ getCurrentTime
      prop "Runs number of times within a *valid* interval" $ prop_interval_alwaysInRange now >> return ()

prop_interval_alwaysInRange n e sc@(Window st en) ioa =
  intervalRestriction n e sc ==> do
    times e sc (ioa :: IO T.Text) `shouldNotReturn` []
prop_interval_alwaysInRange n e sc@(Offset ms) ioa =
  (e >= 0) && (e < 6000) && (ms >= 0) ==> do
    let val = times e sc (ioa :: IO T.Text)
    val `shouldNotReturn` []

intervalRestriction n e sc@(Window st en) = (n <= st) && (n <= en) && (e < 6000)

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
