{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Schedule.ExamplesSpec ( spec ) where
import           Control.Applicative             ((<$>), (<*>))
import           Control.Monad.IO.Class          (liftIO)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime,
                                                  addUTCTime, getCurrentTime)
import           Data.Time.Schedule.Chaos        (Schedule (..),
                                                  mergeAndRunSchedules,
                                                  mkOffsets, runSchedules)
import           Data.Time.Schedule.Chaos.Parser (parseTargets)
import           Test.Hspec

instance Show (IO a) where
  show _ = "IO a"

url = "https://api.coindesk.com/v1/bpi/currentprice/GBP.json"

getBitcoinPrice = do
  return 12

spec :: Spec
spec = parallel $ do
  describe "Examples" $ do
    it "1) query the price of bitcoin every second, 5 times" $ do
      let offsets = take 5 $ mkOffsets 1000 -- Offsets, every 1 second
          -- Now we've 5 (Offset 1000, getBitcoinPrice) pairs, ready to run
      mergeAndRunSchedules offsets (replicate 5 getBitcoinPrice) `shouldNotReturn` []
