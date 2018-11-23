{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Time.Schedule.Chaos.ParserSpec ( spec ) where

import qualified Data.ByteString                 as B
import           Data.ByteString.Char8           (pack)
import           Data.Char                       (toLower)
import           Data.Either                     (either)
import           Data.Time.Schedule.Chaos
import           Data.Time.Schedule.Chaos.Parser
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary TimeUnit where
  arbitrary = elements [Seconds ..]

instance Arbitrary Frequency

spec :: Spec
spec =
  describe "ParserSpec" $
    context "parsePlan - supports all TimeUnit and arbitrary values" $ do
      prop "QuickCheck - values, TimeUnit, Frequency" $
        (\(i :: Int, u :: TimeUnit, fr :: Frequency) -> do
          let ex1 = pack $ "every " ++ show i ++ " " ++ lc u ++ " { touch ./lol }"
          parserTest ex1 i u

          let ex2 = pack $ "in " ++ show i ++ " " ++ lc u ++ " { touch ./lol }"
          parserTest ex2 i u)

      prop "QuickCheck - values, TimeUnit, Frequency and body types" $
        (\(i :: Int, u :: TimeUnit, fr :: Frequency) -> do
          let ex1 = pack $ "every " ++ show i ++ " " ++ lc u ++ " @ http://google.com"
              ex2 = pack $ "in " ++ show i ++ " " ++ lc u ++ " : hello there!"
          parserTest ex1 i u
          parserTest ex2 i u)

lc :: TimeUnit -> String
lc = map toLower . show

parserTest :: B.ByteString -> Int -> TimeUnit -> Frequency -> Expectation
parserTest ex i u f =
  either error onSuccess $ parsePlan ex
    where onSuccess (Plan f' s _) = do
                                f' `shouldBe` f
                                s `shouldBe` Offset (i * (toMillis u))
                                return ()
