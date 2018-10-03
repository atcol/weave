{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Time.Schedule.Chaos.ParserSpec ( spec ) where

import Control.Monad (forM_)

import Data.Char (toLower)
import qualified Data.ByteString as B
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime,
                                                  addUTCTime, getCurrentTime)
import           Data.Time.Schedule.Chaos        (Schedule (..))
import           Data.Time.Schedule.Chaos.Parser 
import           Debug.Trace                     (traceM, traceShow)
import           System.IO.Unsafe                (unsafePerformIO)
import           Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Show (IO a) where
  show _ = "IO a"

instance Eq (IO a) where
  (==) _ _ = True

instance Arbitrary TimeUnit where
  arbitrary = elements [Seconds ..]

spec :: Spec
spec = 
  describe "ParserSpec" $ do 
    context "parseTargets" $ do
      prop "parse: every" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex = pack $ "every " ++ show i ++ " " ++ lc u ++ " { touch ./lol }"
          parserTest ex i u)

      prop "parse: in" $ do
        (\(i :: Int, u :: TimeUnit) -> do
          let x = pack $ "in " ++ show i ++ " " ++ lc u ++ " { date }"
          parserTest x i u)

lc :: TimeUnit -> String
lc = map toLower . show

parserTest :: B.ByteString -> Int -> TimeUnit -> Expectation
parserTest ex i u = 
  case parseTargets ex of
    Left e -> error $ "Failed with " ++ show e
    Right r -> r `shouldBe` [(Offset (i * (toMillis u)), print "lol")]
