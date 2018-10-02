{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Schedule.Chaos.ParserSpec ( spec ) where

import Control.Monad (forM_)

import Data.Char (toLower)
import Data.ByteString.Char8 (pack)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime,
                                                  addUTCTime, getCurrentTime)
import           Data.Time.Schedule.Chaos        (Schedule (..))
import           Data.Time.Schedule.Chaos.Parser 
import           Debug.Trace                     (traceM, traceShow)
import           System.IO.Unsafe                (unsafePerformIO)
import           Test.Hspec

instance Show (IO a) where
  show _ = "IO a"

instance Eq (IO a) where
  (==) _ _ = True

spec :: Spec
spec = parallel $ do
  describe "ParserSpec" $ do
    forM_ [Seconds ..] (\u -> do
      it ("parse: every - " ++ show u) $ do

        case parseTargets "every 5 seconds { touch ./lol }" of
          Left e -> error $ "Failed with " ++ show e
          Right r -> do
            r `shouldNotBe` []
            case (head r) of
              (Offset 5000, _) -> print ""
              a                -> error $ "Incorrect response from parse:" ++ show a

      it ("parse: in - " ++ show u) $ do
        case parseTargets (pack $ "in 1" ++ lc u ++ " { date }") of
          Left e  -> error $ "Failed with " ++ show e
          Right r -> r `shouldBe` [(Offset (toMillis u), print "lol")]

        case parseTargets (pack $ "in 151.1875 " ++ lc u ++ " { date }" ) of
          Left e  -> error $ "Failed with " ++ show e
          Right r -> r `shouldBe` [(Offset ((toMillis u) * 151), print "lol")])

lc :: TimeUnit -> String
lc = map toLower . show
