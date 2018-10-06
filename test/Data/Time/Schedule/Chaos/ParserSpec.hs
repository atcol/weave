{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Time.Schedule.Chaos.ParserSpec ( spec ) where

import           Control.Monad                   (forM_)
import qualified Data.ByteString                 as B
import           Data.ByteString.Char8           (pack)
import           Data.Char                       (toLower)
import           Data.Either                     (either)
import           Data.Time.Schedule.Chaos        (Schedule (..))
import           Data.Time.Schedule.Chaos.Parser
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary TimeUnit where
  arbitrary = elements [Seconds ..]

spec :: Spec
spec =
  describe "ParserSpec" $
    context "parseTargets - units" $ do
      prop "QuickCheck - millis & TimeUnit" $
        (\(i :: Int, u :: TimeUnit) -> do
          let ex = pack $ "every " ++ show i ++ " " ++ lc u ++ " { touch ./lol }"
          parserTest ex i u

          let x = pack $ "in " ++ show i ++ " " ++ lc u ++ " { touch ./lol }"
          parserTest x i u)

lc :: TimeUnit -> String
lc = map toLower . show

parserTest :: B.ByteString -> Int -> TimeUnit -> Expectation
parserTest ex i u =
  either error onSuccess $ parseTargets ex
    where onSuccess r = fst r `shouldBe` Offset (i * (toMillis u))
