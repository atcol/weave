{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | The Chaos parsing API
module Data.Time.Schedule.Chaos.Parser (
  TimeUnit (..),

  parseTargets,
  scheduleP,
  toMillis
  ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B
import           Data.Time.Schedule.Chaos         (Schedule (..))
import           Prelude                          hiding (takeWhile)
import           System.Process                   (callCommand)

-- The unit of time for schedule expressions
data TimeUnit = Seconds
              | Minutes
              | Hours
              | Days
              deriving (Enum, Eq, Show)

parseTargets :: B.ByteString -> Either String (Schedule, IO ())
parseTargets = wrap . parseOnly chaosP
  where wrap (Left e) = Left $ "Parse error: " ++ show e -- For testing
        wrap r        = r

chaosP :: Parser (Schedule, IO ())
chaosP = do
  sch <- scheduleP
  bdy <- bodyP
  return (sch, mkAction bdy)
    where mkAction b = callCommand $ B.unpack b

-- | Parse a schedule
scheduleP :: Parser Schedule
scheduleP = do
  fn <- scheduleCtorP <?> "Schedule Parser"
  num <- fmap round double
  skipSpace
  tu <- unitP <?> "TimeUnit Parser"
  skipSpace
  return $ fn $ num * (toMillis tu)

-- | Parse the schedule string from plain English to its corresponding data constructor
scheduleCtorP :: Parser (Int -> Schedule)
scheduleCtorP = do
  ctorStr <- (string "every" <|> string "in") <?> "Schedule ctor Parser"
  skipSpace
  case ctorStr of
    "every" -> return Offset
    "in"    -> return Offset

-- | Parse a @TimeUnit@ from plain English
unitP :: Parser TimeUnit
unitP = do
  ctorStr <- (string "seconds"
              <|> string "minutes"
              <|> string "hours"
              <|> string "days") <?> "Unit ctor Parser"
  skipSpace
  case ctorStr of
    "seconds" -> return Seconds
    "minutes" -> return Minutes
    "hours"   -> return Hours
    "days"    -> return Days
    _         -> error "Unkown schedule token"

-- | Parse a full command body, e.g. between '{' and '}'
bodyP :: Parser B.ByteString
bodyP = do
  op <- (char '{' <?> "Open brace") <|>
          (char '@' <?> "URL") <|>
          (char ':' <?> "Plain text")
  skipSpace
  -- Will this fail on embedded } ?
  takeWhile (/= (inverse op)) <?> "Body contents Parser"
    where inverse '{' = '}'
          inverse '@' = '\n'
          inverse ':' = '\n'

-- | Represent our @TimeUnit@ as an @Int@
toMillis :: TimeUnit -> Int
toMillis Seconds = 1000
toMillis Minutes = (toMillis Seconds) * 60
toMillis Hours   = (toMillis Minutes) * 60
toMillis Days    = (toMillis Hours) * 24
