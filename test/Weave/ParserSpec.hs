{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Weave.ParserSpec ( spec ) where

import           Data.Attoparsec.Text  (parseOnly)
import           Data.Char             (toLower)
import           Data.Either           (isLeft, isRight)
import qualified Data.Text             as T
import           Prelude               (error)
import           Protolude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       hiding (Success)
import           Weave
import           Weave.Parser

instance Arbitrary TimeUnit where
  arbitrary = elements [Seconds ..]

spec :: Spec
spec =
  describe "ParserSpec" $ do
    context "Hardcoded examples" $ do
      it "every 2s date | wc" $ do
        parserTest "every 2s date | wc" 2 Seconds Continuous

    context "parsePlan - supports all TimeUnit and arbitrary values" $ do
      prop "QuickCheck - values, TimeUnit, Frequency" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex1 = T.concat ["every ", T.pack $ show i, " ", lc u, " { touch ./lol }"]
          parserTest ex1 i u Continuous

          let ex2 = T.concat ["in ", show i, " ", lc u, " { touch ./lol }"]
          parserTest ex2 i u Once)

      prop "QuickCheck - values, TimeUnit, Frequency and body types" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex1 = T.concat ["every ", show i, " ", lc u, " @ http://google.com"]
              ex2 = T.concat ["in ", show i, " ", lc u, " : hello there!"]
          parserTest ex1 i u Continuous
          parserTest ex2 i u Once)

    context "temporalP - failes on anything but supported units" $ do
      prop "QuickCheck - doubles and chars" $
        (\(i :: Double, u :: Char) -> do
          let ex1 = T.concat ["in ", T.pack $ show i, T.singleton u]
              ex2 = T.concat ["every ", T.pack $ show i, T.singleton u]
          if (elem u supportedUnits) then
            (parseOnly temporalP ex1) `shouldSatisfy` isRight
          else
            (parseOnly temporalP ex1) `shouldSatisfy` isLeft)

lc :: TimeUnit -> T.Text
lc = T.pack . map toLower . show

parserTest :: T.Text -> Int -> TimeUnit -> Frequency -> Expectation
parserTest ex i u f =
  onSuccess $ parsePlan ex
    where onSuccess (MalformedPlan e) = error $ T.unpack e
          onSuccess (Success (Plan [])) = error "Empty plan"
          onSuccess (Success (Plan ((Temporal fr s _):_))) = do
                                fr `shouldBe` f
                                s `shouldBe` Offset (i * (toMillis u))
