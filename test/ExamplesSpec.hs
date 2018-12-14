{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module ExamplesSpec ( spec ) where

import           Control.Monad    (filterM, forM_, mapM)
import           Data.List
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           System.Directory
import           Test.Hspec
import           Weave
import           Weave.Parser     (parsePlan)

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

getExamples :: FilePath -> IO [T.Text]
getExamples p = getDirectoryContents p
  >>= filterM (return . isWeaveFile)
  >>= mapM (T.readFile . absPath)
    where isWeaveFile = isSuffixOf ".weave"
          absPath f = p ++ "/" ++ f

validOperator :: FilePath -> Either String (Plan a) -> Expectation
validOperator _ _ = pendingWith "To-do"

validOffset :: FilePath -> Either String (Plan a) -> Expectation
validOffset _ (Left l)           = error $ "Parse error: " ++ show l
validOffset _ (Right (Plan s _)) = (lengthIs s 1) >> ((snd (head s)) `shouldNotBe` Offset 0)

validFrequency :: FilePath -> Either String (Plan a) -> Expectation
validFrequency _ (Left l)            = error $ "Parse error: " ++ show l
validFrequency p (Right (Plan s _)) = (lengthIs s 1) >> ((fst (head s)) `shouldBe` (getFrequency p))
  where getFrequency _ = Once --FIXME derive this from the filename

shouldNotParse :: FilePath -> Either String (Plan a) -> Expectation
shouldNotParse _ (Right (Plan _ s)) = error $ "Failure expected: " ++ show s
shouldNotParse _ (Left l)             = l `shouldSatisfy` isInfixOf "Parse error"

lengthIs :: [a] -> Int -> Expectation
lengthIs x n = length x `shouldBe` n

runTest :: FilePath -> ValidationFunction -> Expectation
runTest p f = do
  exs <- getExamples p
  length exs `shouldNotBe` 0
  forM_ exs (\ex -> do
    print $ "Testing: " ++ show ex
    f p $ parsePlan ex)
