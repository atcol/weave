{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Time.Schedule.ExamplesSpec ( spec ) where
import           Control.Monad                   (filterM, forM_, mapM)
import qualified Data.ByteString.Char8           as BS
import           Data.List
import           Data.Time.Schedule.Chaos        (Schedule (..))
import           Data.Time.Schedule.Chaos.Parser (parseTargets)
import           System.Directory
import           Test.Hspec

type Target = (Schedule, IO ())

type ValidationFunction = (Either String Target) -> Expectation

spec :: Spec
spec = do
  describe "Parser" $ do
    it "Supports valid examples" $
      runTest "./examples/valid" validParseTest
    it "Rejects invalid examples" $ do
      v <- runTest "examples/invalid" invalidParseTest
      return v

getExamples :: FilePath -> IO [BS.ByteString]
getExamples p = getDirectoryContents p
  >>= filterM (return . isChaosFile)
  >>= mapM (BS.readFile . absPath)
    where isChaosFile = isSuffixOf ".chaos"
          absPath f = p ++ "/" ++ f

validParseTest :: (Either String Target) -> Expectation
validParseTest (Right (s, _)) = s `shouldNotBe` Offset 0
validParseTest (Left l)       = error $ "Parse error: " ++ show l

invalidParseTest :: (Either String Target) -> Expectation
invalidParseTest (Right (s, _)) = error $ "Failure expected: " ++ show s
invalidParseTest (Left l)       = l `shouldSatisfy` isInfixOf "Parse error"

runTest :: FilePath -> ValidationFunction -> Expectation
runTest p f = do
  exs <- getExamples p
  length exs `shouldNotBe` 0
  forM_ exs (f . parseTargets)
