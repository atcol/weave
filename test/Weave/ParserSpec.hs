{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Weave.ParserSpec ( spec ) where

import           Data.Attoparsec.Text
import           Data.Char             (toLower)
import           Data.Either           (isLeft, isRight)
import           Data.Maybe            (fromJust)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Prelude               as PR
import           Protolude
import           System.IO             (nativeNewline)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       hiding (Success)
import           Utils
import           Weave
import           Weave.Parser

instance Arbitrary TimeUnit where
  arbitrary = elements [Seconds ..]

example_twoSchedules = "in 2s AB\nevery 5s CDEF"

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

    context "statementP - valid" $ do
      it "Should parse multiple" $ do
        case parseOnly (many1 $ statementP []) example_twoSchedules of
          Right l  -> do
            traceM $ show l
            length l `shouldBe` 2
            let (Temporal f1 s1 _) = fromJust $ head l
                (Temporal f2 s2 _) = PR.last l
            expectTemporal Once (Offset 2000) f1 s1
            expectTemporal Continuous (Offset 5000) f2 s2
          e -> PR.error $ "Parsing error " ++ show e

    context "temporalP - failes on anything but supported units" $ do
      prop "QuickCheck - doubles and chars" $
        (\(i :: Double, u :: Char) -> do
          let ex1 = T.concat ["in ", T.pack $ show i, T.singleton u]
              ex2 = T.concat ["every ", T.pack $ show i, T.singleton u]
          if (elem u supportedUnits) then
            (parseOnly temporalP ex1) `shouldSatisfy` isRight
          else
            (parseOnly temporalP ex1) `shouldSatisfy` isLeft)

    context "actionExpressionsP" $ do
      it "Should parse multiples" $ do
        validateParseOnly (actionExpressionsP []) "A\n" $ (\r -> do
            r `shouldBe` [(Action Shell "A" "A", Sequence)])

        validateParseOnly (actionExpressionsP []) "A | B\n" $ (\r -> do
            r `shouldBe` [(Action Shell "A" "A", Pipe),
                          (Action Shell "B" "B", Sequence)])

        validateParseOnly (actionExpressionsP []) "A & B | C\n" $ (\r -> do
            r `shouldBe` [(Action Shell "A" "A", And)
                         , (Action Shell "B" "B", Pipe)
                         , (Action Shell "C" "C", Sequence)])
validateParseOnly :: Show a => Parser a -> Text -> (a -> Expectation) -> Expectation
validateParseOnly p t e =
  case (parseOnly p t) of
      Right r -> e r
      er      -> PR.error $ "Parsing error " ++ show er

lc :: TimeUnit -> T.Text
lc = T.pack . map toLower . show

parserTest :: T.Text -> Int -> TimeUnit -> Frequency -> Expectation
parserTest ex i u f =
  onSuccess $ parsePlan ex
    where onSuccess (MalformedPlan e) = PR.error $ T.unpack e
          onSuccess (Success (Plan [])) = PR.error "Empty plan"
          onSuccess (Success (Plan ((Temporal fr s _):_))) = do
                                fr `shouldBe` f
                                s `shouldBe` Offset (i * (toMillis u))
