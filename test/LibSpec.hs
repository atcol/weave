module LibSpec ( spec ) where

import           Lib                    (randomSeconds)
import           System.Random          (RandomGen)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Random

spec :: Spec
spec = do
  describe "genTime" $ do
    context "randomSeconds" $
      it "" $ property $ prop_alwaysWithinRange

    --it "Produces time within schedule boundaries" $ property $

prop_alwaysWithinRange :: RandomGen g => g -> Int -> Bool
prop_alwaysWithinRange rg i = (i == 0) || ((i >= 0) && ((floor $ randomSeconds rg i) <= i))
