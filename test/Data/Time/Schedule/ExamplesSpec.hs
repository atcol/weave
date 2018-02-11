{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Schedule.ExamplesSpec ( spec ) where
import           Control.Applicative      ((<$>), (<*>))
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (Reader, asks)
import           Data.Time.Clock          (NominalDiffTime, UTCTime, addUTCTime,
                                           getCurrentTime)
import           Data.Time.Schedule.Chaos (Schedule (..), mkOffsets,
                                           runSchedules)
import           Debug.Trace              (traceM, traceShow)
import qualified Network.Http.Client      as N
import qualified System.IO.Streams        as S
import           System.IO.Unsafe         (unsafePerformIO)
import           System.Process           (callCommand)
import           System.Random            (RandomGen, newStdGen)
import           Test.Hspec

getBitcoinPrice = do
  --get "https://api.coindesk.com/v1/bpi/currentprice/GBP.json" (\_ i -> S.connect i S.stdout)
  x <- N.get "https://api.coindesk.com/v1/bpi/currentprice/GBP.json" N.concatHandler
  print x
  return x

spec :: Spec
spec = parallel $ do
  describe "Examples" $
    it "1) query the price of bitcoin every second, 5 times" $ do
      let offsets = mkOffsets 1000 -- Offsets, every 1 second
          pairs = take 5 $ fmap (\o -> (o, getBitcoinPrice)) offsets
      -- Now we've 5 (Offset 1000, getBitcoinPrice) pairs, ready to run
      runSchedules pairs `shouldNotReturn` []
