{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Time.Schedule.ExamplesSpec ( spec ) where
import           Control.Monad                   (filterM, forM_, mapM)
import qualified Data.ByteString.Char8           as BS
import           Data.List
import           Data.Time.Schedule.Chaos
import           Data.Time.Schedule.Chaos.Parser (parsePlan)
import           System.Directory
import           Test.Hspec

type ValidationFunction = FilePath -> Either String (Plan ()) -> Expectation

spec :: Spec
spec = do
  describe "Parser" $ do
    context "Supports valid examples" $ do

      it "General" $
        runTest "./examples/valid" validOffsetTest

      it "Operators" $
        runTest "./examples/valid/frequency" validFrequencyTest

    context "Rejects invalid examples" $ do
      it "Fails with a parse error" $
        runTest "examples/invalid" shouldNotParseTest

getExamples :: FilePath -> IO [BS.ByteString]
getExamples p = getDirectoryContents p
  >>= filterM (return . isChaosFile)
  >>= mapM (BS.readFile . absPath)
    where isChaosFile = isSuffixOf ".chaos"
          absPath f = p ++ "/" ++ f

validOffsetTest :: FilePath -> Either String (Plan ()) -> Expectation
validOffsetTest _ (Left l)             = error $ "Parse error: " ++ show l
validOffsetTest _ (Right (Plan _ s _)) = s `shouldNotBe` Offset 0

validFrequencyTest :: FilePath -> Either String (Plan ()) -> Expectation
validFrequencyTest _ (Left l)              = error $ "Parse error: " ++ show l
validFrequencyTest p (Right (Plan fr _ _)) = fr `shouldBe` (getFrequency p)
  where getFrequency = read . takeWhile ((/=) '.')

shouldNotParseTest :: FilePath -> Either String (Plan ()) -> Expectation
shouldNotParseTest _ (Right (Plan _ s _)) = error $ "Failure expected: " ++ show s
shouldNotParseTest _ (Left l)             = l `shouldSatisfy` isInfixOf "Parse error"

runTest :: FilePath -> ValidationFunction -> Expectation
runTest p f = do
  exs <- getExamples p
  length exs `shouldNotBe` 0
  forM_ exs (\ex -> do
    print $ "Testing: " ++ show ex
    f p $ parsePlan ex)
