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

type ValidationFunction = (Either String (Plan ())) -> Expectation

spec :: Spec
spec = do
  describe "Parser" $ do
    it "Supports valid examples" $
      runTest "./examples/valid" validParseTest
    it "Rejects invalid examples" $ do
      runTest "examples/invalid" invalidParseTest

getExamples :: FilePath -> IO [BS.ByteString]
getExamples p = getDirectoryContents p
  >>= filterM (return . isChaosFile)
  >>= mapM (BS.readFile . absPath)
    where isChaosFile = isSuffixOf ".chaos"
          absPath f = p ++ "/" ++ f

validParseTest :: (Either String (Plan ())) -> Expectation
validParseTest (Right (Plan fr s _)) = s `shouldNotBe` Offset 0
validParseTest (Left l)              = error $ "Parse error: " ++ show l

invalidParseTest :: (Either String (Plan ())) -> Expectation
invalidParseTest (Right (Plan fr s _)) = error $ "Failure expected: " ++ show s
invalidParseTest (Left l)       = l `shouldSatisfy` isInfixOf "Parse error"

runTest :: FilePath -> ValidationFunction -> Expectation
runTest p f = do
  exs <- getExamples p
  length exs `shouldNotBe` 0
  forM_ exs (\ex -> do
    print $ "Testing: " ++ show ex
    f $ parsePlan ex)
