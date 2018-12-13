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
    context "Valid examples" $ do

      it "General" $ do
        runTest "./examples/valid" validOffset

        runTest "./examples/valid/frequency" validFrequency

      it "Operators" $
        runTest "./examples/valid/operators" validOperator

    context "Invalid examples" $ do
      it "Fails with a parse error" $
        runTest "examples/invalid" shouldNotParse

getExamples :: FilePath -> IO [BS.ByteString]
getExamples p = getDirectoryContents p
  >>= filterM (return . isChaosFile)
  >>= mapM (BS.readFile . absPath)
    where isChaosFile = isSuffixOf ".chaos"
          absPath f = p ++ "/" ++ f

validOperator :: FilePath -> Either String (Plan ()) -> Expectation
validOperator _ _ = pendingWith "To-do"

validOffset :: FilePath -> Either String (Plan ()) -> Expectation
validOffset _ (Left l)             = error $ "Parse error: " ++ show l
validOffset _ (Right (Plan _ s _)) = s `shouldNotBe` Offset 0

validFrequency :: FilePath -> Either String (Plan ()) -> Expectation
validFrequency _ (Left l)              = error $ "Parse error: " ++ show l
validFrequency p (Right (Plan fr _ _)) = fr `shouldBe` (getFrequency p)
  where getFrequency _ = Once --FIXME derive this from the filename

shouldNotParse :: FilePath -> Either String (Plan ()) -> Expectation
shouldNotParse _ (Right (Plan _ s _)) = error $ "Failure expected: " ++ show s
shouldNotParse _ (Left l)             = l `shouldSatisfy` isInfixOf "Parse error"

runTest :: FilePath -> ValidationFunction -> Expectation
runTest p f = do
  exs <- getExamples p
  length exs `shouldNotBe` 0
  forM_ exs (\ex -> do
    print $ "Testing: " ++ show ex
    f p $ parsePlan ex)
