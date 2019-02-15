{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Weave.ParserSpec ( spec ) where

import           Data.Attoparsec.Text
import           Data.Char               (toLower)
import           Data.Either             (isLeft, isRight)
import           Data.Maybe              (fromJust)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Prelude                 as PR
import           Protolude
import           System.IO               (nativeNewline)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck         hiding (Success)
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
        parserTest "every 2s date | wc" 2 Seconds Continuous 2

    context "parsePlan" $ do
      prop "QuickCheck - Frequency & TimeUnit" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex1 = T.concat ["every ", T.pack $ show i, lc u, " { touch ./lol }"]
              ex2 = T.concat ["in ", show i, lc u, " { touch ./lol }"]
          parserTest ex1 i u Continuous 1
          parserTest ex2 i u Once 1)

      prop "QuickCheck - Frequency, TimeUnit and body types" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex1 = T.concat ["every ", show i, lc u, " @ http://google.com"]
              ex2 = T.concat ["in ", show i, lc u, " { echo 'hello there!' }"]
          parserTest ex1 i u Continuous 1
          parserTest ex2 i u Once 1)

    context "statementP - valid" $ do
      prop "QuickCheck - every Freq. and body combo" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex1 = T.concat ["every ", show i, lc u, " @ http://google.com", "\n"]
              ex2 = T.concat ["in ", show i, lc u, " @ http://google.com", "\n"]
              ex3 = T.concat ["every ", show i, lc u, " { ls -la }", "\n"]
              ex4 = T.concat ["in ", show i, lc u, " { ls -la }", "\n"]
              ex5 = T.concat ["every ", show i, lc u, " : this is cool!", "\n"]
              ex6 = T.concat ["in ", show i, lc u, " : this is cool!", "\n"]

          validateParseOnly (statementP []) (ex1) $ (\s -> do
            s `shouldBe` (Temporal Continuous (Offset (i * (toMillis u)))
                          [(Action Shell "inline" " http://google.com", Sequence)]))

          validateParseOnly (statementP []) (ex2) $ (\s -> do
            s `shouldBe` (Temporal Once (Offset (i * (toMillis u)))
                          [(Action Shell "inline" " http://google.com", Sequence)]))

          validateParseOnly (statementP []) (ex3) $ (\s -> do
            s `shouldBe` (Temporal Continuous (Offset (i * (toMillis u)))
                          [(Action Shell "inline" " ls -la ", Sequence)]))

          validateParseOnly (statementP []) (ex4) $ (\s -> do
            s `shouldBe` (Temporal Once (Offset (i * (toMillis u)))
                          [(Action Shell "inline" " ls -la ", Sequence)]))

          validateParseOnly (statementP []) (ex5) $ (\s -> do
            s `shouldBe` (Temporal Continuous (Offset (i * (toMillis u)))
                          [(Action Shell "inline" " this is cool!", Sequence)]))

          validateParseOnly (statementP []) (ex6) $ (\s -> do
            s `shouldBe` (Temporal Once (Offset (i * (toMillis u)))
                          [(Action Shell "inline" " this is cool!", Sequence)])))

      it "Should parse multiple" $ do
        validateParseOnly (many1 $ statementP []) example_twoSchedules $ (\l -> do
          traceM $ show l
          length l `shouldBe` 2
          let (Temporal f1 s1 _) = fromJust $ head l
              (Temporal f2 s2 _) = PR.last l
          expectTemporal Once (Offset 2000) f1 s1
          expectTemporal Continuous (Offset 5000) f2 s2)

    context "temporalP - supported units" $ do
      prop "QuickCheck - doubles and chars" $
        (\(i :: Int) ->
          forM_ supportedUnits (\u -> do
            let ex1 = T.concat ["in ", T.pack $ show i, T.singleton u]
                ex2 = T.concat ["every ", T.pack $ show i, T.singleton u]
            (parseOnly temporalP ex1) `shouldSatisfy` isRight))

    context "actionExpressionsP" $ do
      it "Should parse multiples" $ do

        validateParseOnly (actionExpressionsP []) "A\n" $ (\r -> do
            r `shouldBe` [ (Action Shell "A" "A", Sequence)])

        validateParseOnly (actionExpressionsP []) "A | B\n" $ (\r -> do
            r `shouldBe` [ (Action Shell "A" "A", Pipe)
                         , (Action Shell "B" "B", Sequence)])

        validateParseOnly (actionExpressionsP []) "A & B | C\n" $ (\r -> do
            r `shouldBe` [ (Action Shell "A" "A", And)
                         , (Action Shell "B" "B", Pipe)
                         , (Action Shell "C" "C", Sequence)])

validateParseOnly :: Show a => Parser a -> Text -> (a -> Expectation) -> Expectation
validateParseOnly p t e =
  case (parseOnly p t) of
      Right r -> e r
      er      -> PR.error $ "Parsing error " ++ show er ++ " - " ++ show t

lc :: TimeUnit -> T.Text
lc = T.toLower . T.singleton . PR.head . PR.show

parserTest :: T.Text -> Int -> TimeUnit -> Frequency -> Int -> Expectation
parserTest ex i u f n =
  onSuccess $ parsePlan ex
    where onSuccess (MalformedPlan e) = PR.error $ T.unpack e
          onSuccess (Success (Plan [])) = PR.error $ "Empty plan: " ++ show ex
          onSuccess (Success (Plan ((Temporal fr s acts):_))) = do
                                fr `shouldBe` f
                                s `shouldBe` Offset (i * (toMillis u))
                                length acts `shouldBe` n
          onSuccess p = PR.error $ "Unrecogniesd parse: "  ++ show p
