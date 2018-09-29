{-# LANGUAGE OverloadedStrings #-}
-- | The Chaos parsing API
module Data.Time.Schedule.Chaos.Parser (
  parseTargets,
  scheduleP
  ) where

import           Control.Applicative              ((<$>), (<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B
import           Data.Char                        (digitToInt)
import           Data.Time.Schedule.Chaos         (Schedule (..))
import           Prelude                          hiding (takeWhile)
import           System.Process                   (callCommand)

-- The unit of time for schedule expressions
data TimeUnit = Seconds | Hours deriving (Eq, Show)

parseTargets :: B.ByteString -> Either B.ByteString [(Schedule, IO ())]
parseTargets s = do
  case parseOnly chaosP s of
    Left a  -> error a
    Right s -> Right s

chaosP :: Parser [(Schedule, IO ())]
chaosP = do
  sch <- scheduleP
  char '{' <?> "Open brace"
  skipSpace
  -- Will this fail on embedded } ?
  cmd <- takeWhile (/= '}') <?> "Command Parser"
  return $ map (\sc -> (sc, callCommand $ B.unpack cmd)) sch

scheduleP :: Parser [Schedule]
scheduleP = do
  fns <- scheduleCtorP <?> "Schedule Parser"
  num <- fmap round double -- digitToInt <$> (many2 digit) --FIXME need multi-digits
  skipSpace
  timeUnit <- unitP <?> "Unit Parser"
  skipSpace
  case timeUnit of
    Seconds -> return $ map (\fn -> fn $ num * 1000) fns -- to milliseconds
    --Hours   -> return $ fn $ num * 1000 * 60 -- to millis, in hours

scheduleCtorP :: Parser ([Int -> Schedule])
scheduleCtorP = do
  ctorStr <- (string "every" <|> string "in") <?> "Schedule ctor Parser"
  space
  case ctorStr of
    "every" -> return $ repeat Offset
    "in"    -> return [Offset]

unitP :: Parser TimeUnit
unitP = do
  ctorStr <- (string "seconds" <|> string "hours") <?> "Unit ctor Parser"
  space
  case ctorStr of
    "seconds" -> return Seconds
    "hours"   -> return Hours
    _         -> error "Unkown schedule token"

