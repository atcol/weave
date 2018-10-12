{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Time.Schedule.ExamplesSpec ( spec ) where
import           Control.Monad                   (filterM, mapM)
import qualified Data.ByteString.Char8           as BS
import           Data.List
import           Data.Time.Schedule.Chaos        (Schedule (..))
import           Data.Time.Schedule.Chaos.Parser (parseTargets)
import           System.Directory
import           Test.Hspec

type Target = (Schedule, IO ())

spec :: Spec
spec = do
  describe "Parser" $ do
    it "Parses valid examples" $ do
      exs <- getExamples "./examples/valid"
      let _ = map (parseTest . parseTargets) exs
      return ()

getExamples :: FilePath -> IO [BS.ByteString]
getExamples p = getDirectoryContents p
  >>= filterM (return . isChaosFile)
  >>= mapM (BS.readFile . absPath)
    where isChaosFile = isSuffixOf ".chaos"
          absPath f = p ++ "/" ++ f

parseTest :: (Either String Target) -> Expectation
parseTest (Right (s, _)) = s `shouldNotBe` Offset 0
parseTest (Left l)       = error $ "Parse error: " ++ show l
