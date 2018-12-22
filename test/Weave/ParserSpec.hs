{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Weave.ParserSpec ( spec ) where

import           Data.Char             (toLower)
import           Data.Either           (either)
import qualified Data.Text             as T
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Weave
import           Weave.Parser

instance Arbitrary TimeUnit where
  arbitrary = elements [Seconds ..]

spec :: Spec
spec =
  describe "ParserSpec" $
    context "parsePlan - supports all TimeUnit and arbitrary values" $ do
      prop "QuickCheck - values, TimeUnit, Frequency" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex1 = T.pack $ "every " ++ show i ++ " " ++ lc u ++ " { touch ./lol }"
          parserTest ex1 i u Continuous

          let ex2 = T.pack $ "in " ++ show i ++ " " ++ lc u ++ " { touch ./lol }"
          parserTest ex2 i u Once)

      prop "QuickCheck - values, TimeUnit, Frequency and body types" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex1 = T.pack $ "every " ++ show i ++ " " ++ lc u ++ " @ http://google.com"
              ex2 = T.pack $ "in " ++ show i ++ " " ++ lc u ++ " : hello there!"
          parserTest ex1 i u Continuous
          parserTest ex2 i u Once)

lc :: TimeUnit -> String
lc = map toLower . show

parserTest :: T.Text -> Int -> TimeUnit -> Frequency -> Expectation
parserTest ex i u f =
  either error onSuccess $ parsePlan ex
    where onSuccess (Plan []) = error "Empty plan"
          onSuccess (Plan ((Temporal fr s _):_)) = do
                                fr `shouldBe` f
                                s `shouldBe` Offset (i * (toMillis u))
