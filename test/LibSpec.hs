module LibSpec ( spec ) where

import           Data.Time.Clock        (NominalDiffTime)
import           Lib                    (randomSeconds)
import           System.Random          (RandomGen, newStdGen)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.IO     ()
import           Test.QuickCheck.Random

mx :: Int
mx = 10000

spec :: Spec
spec = do
  describe "genTime" $ do
    context "randomSeconds" $ do
      it "Always produces times within range" $ do
        --randomSeconds g mx `shouldSatisfy` (validRange mx)
        property prop_validRange

--prop_validRange :: RandomGen g => g -> Int -> (NominalDiffTime -> Bool)
prop_validRange v = (v > 0) ==> do
  g <- newStdGen
  randomSeconds g v `shouldSatisfy` validRange

validRange :: NominalDiffTime -> Bool
validRange v = (v >= 0) && (v <= (realToFrac mx))
