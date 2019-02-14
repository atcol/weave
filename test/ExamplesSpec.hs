{-# OPTIONS_GHC -Wall #-}

module ExamplesSpec ( spec ) where

import           Control.Monad    (filterM, forM_, mapM_)
import qualified Data.List        as L
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Prelude          (error)
import           Protolude
import           System.Directory
import           Test.Hspec
import           Utils
import           Weave
import           Weave.Parser     (ParseResult (..), parsePlan)

type ValidationFunction = FilePath -> ParseResult -> Expectation

spec :: Spec
spec = do
  describe "Parser" $ do
    context "Valid examples" $ do

      it "General" $ do
        runTest "./examples" validOffset

        runTest "./examples/frequency" validFrequency


      it "Explicit Tests" $ do

        plan <- T.readFile "examples/frequency/multiple.weave" >>= return . parsePlan
        case plan of
          MalformedPlan e -> error $ "Parsing shouldn't have failed" ++ show e
          Success (Plan l) -> do
            length l `shouldBe` 2
            head l `shouldSatisfy` (\p -> correctStmt (N 1) (Offset 1000) $ getStmt p)
              where getStmt (Just s) = s
                    getStmt _        = error "No plan parsed"

      it "Operators" $
        runTest "./examples/operators" validOffset

getExamples :: FilePath -> IO [(FilePath, T.Text)]
getExamples p = getDirectoryContents p
  >>= filterM (return . isWeaveFile)
  >>= mapM (\f -> (T.readFile $ absPath f) >>= (\r -> return (f, r)))
    where isWeaveFile = L.isSuffixOf ".weave"
          absPath f = p ++ "/" ++ f

validOperator :: FilePath -> ParseResult -> Expectation
validOperator _ _ = pendingWith "To-do"

validOffset :: FilePath -> ParseResult -> Expectation
validOffset _ (MalformedPlan l)  = error $ "Parse error: " ++ show l
validOffset _ (Success (Plan s)) = (lengthIs s 1) >>
  mapM_ scheduleIsPositive s

validFrequency :: FilePath -> ParseResult -> Expectation
validFrequency _ (MalformedPlan l)  = error $ "Parse error: " ++ show l
validFrequency p (Success (Plan s)) = (lengthIs s 1) >>
  (mapM_ (\(Temporal f _ _) -> f `shouldBe` getFrequency p) s)
  where getFrequency _ = Once --FIXME derive this from the filename

shouldNotParse :: FilePath -> ParseResult -> Expectation
shouldNotParse _ (Success p) = error $ "No statements should be parsed: " ++ show p
shouldNotParse _ (MalformedPlan l)         = l `shouldSatisfy` T.isInfixOf "Parse error"

emptyParse :: FilePath -> ParseResult -> Expectation
emptyParse _ (Success (Plan s)) = length s `shouldBe` 0

lengthIs :: [a] -> Int -> Expectation
lengthIs x n = length x `shouldBe` n

scheduleIsPositive (Temporal _ (Offset n) _) = n `shouldSatisfy` ((<) 0)

runTest :: FilePath -> ValidationFunction -> Expectation
runTest p f = do
  exs <- getExamples p
  length exs `shouldNotBe` 0
  forM_ exs (\ex -> do
    print $ "Testing (" ++ fst ex ++ "): " ++ (show $ snd ex)
    f p $ parsePlan $ snd ex)
