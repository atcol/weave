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
import           Data.Maybe                       (catMaybes)
import           Data.Time.Schedule.Chaos         (Schedule (..))
import           Debug.Trace                      (traceM)
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
  bdys <- many' (declaredBodyP <|> (bodyP >>= (\a -> return ("inline", a))))
  case bdys of
    [] -> do
      s <- scheduleP
      b <- bodyP
      return (s, mkAction b)
    l -> do
      s <- scheduleP
      refs <- bodyRefP l `sepBy` (char ',')
      let resolvedActions = catMaybes refs
      return (s, sequence_ $ catMaybes refs)

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
  skipWhile ((==) '\n')
  ctorStr <- ((string "every" <?> "every") <|> (string "in" <?> "in")) <?> "Schedule ctor Parser"
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

-- | Parse an action with its identifier
declaredBodyP :: Parser (B.ByteString, B.ByteString)
declaredBodyP = do
  act <- string "action" <?> "Declared Action"
  skipSpace
  name <- many1 letter_ascii
  skipSpace
  bdy <- bodyP
  return (B.pack name, bdy)

-- | Parse a full command body, e.g. between '{' and '}'
bodyP :: Parser B.ByteString
bodyP = do
  op <- (char '{' <?> "Open brace") <|>
          (char '@' <?> "URL") <|>
          (char ':' <?> "Plain text")
  skipSpace
  -- Will this fail on embedded } ?
  res <- takeWhile (/= (inverse op)) <?> "Body contents Parser"
  skipMany (char $ inverse op)
  return res
    where inverse '{' = '}'
          inverse '@' = '\n'
          inverse ':' = '\n'

-- | Parse the body reference (e.g. "action1, action2") to an action
-- after looking up the identifier in the given list
bodyRefP :: [(B.ByteString, B.ByteString)] -> Parser (Maybe (IO ()))
bodyRefP [] = return Nothing
bodyRefP l = do
  skipSpace
  iden <- many1 letter_ascii
  toAction $ filter ((==) iden . B.unpack) $ map fst l
    where toAction []    = return Nothing
          toAction (a:_) = return $ Just $ mkAction a

-- | Represent our @TimeUnit@ as an @Int@
toMillis :: TimeUnit -> Int
toMillis Seconds = 1000
toMillis Minutes = (toMillis Seconds) * 60
toMillis Hours   = (toMillis Minutes) * 60
toMillis Days    = (toMillis Hours) * 24

mkAction :: B.ByteString -> IO ()
mkAction  = callCommand . B.unpack
