{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | The Chaos parsing API
module Data.Time.Schedule.Chaos.Parser (
  Plan (..),
  TimeUnit (..),

  parsePlan,
  scheduleP,
  toMillis
  ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B
import           Data.Maybe                       (catMaybes)
import           Data.Time.Schedule.Chaos         (Frequency (..), Plan (..),
                                                   Schedule (..))
import           Prelude                          hiding (takeWhile)
import           System.Process                   (callCommand)

-- The unit of time for schedule expressions
data TimeUnit = Seconds
              | Minutes
              | Hours
              | Days
              deriving (Enum, Eq, Show)

-- | A wrapper for actions/behaviour
data Action a =
              -- | A basic shell
              Shell String B.ByteString
              -- | Reference another action
              | Ref String (Action a)
              deriving (Eq, Show)

parsePlan :: B.ByteString -> Either String (Plan ())
parsePlan = wrap . parseOnly chaosP
  where wrap (Left e) = Left $ "Parse error: " ++ show e -- For testing
        wrap r        = r

chaosP :: Parser (Plan ())
chaosP = do
  many' declaredActionP >>= mapBdys
    where mapBdys [] = do -- parse the body inline
            (fr, s) <- scheduleP
            b <- bodyP --FIXME need a classifier or value ctor
            return $ Plan fr s $ mkAction s (Shell "inline" b)
          mapBdys l = do
            (fr, s) <- scheduleP
            refs <- bodyRefP s l `sepBy` (char ',')
            let resolvedActions = catMaybes refs
            case resolvedActions of
              [] -> fail "Parse error: No actions found by references"
              _  -> return $ Plan fr s $ sequence_ $ map snd resolvedActions

-- | Parse a schedule
scheduleP :: Parser (Frequency, Schedule)
scheduleP = do
  (fr, fn) <- scheduleCtorP <?> "Schedule Parser"
  num <- fmap round double
  skipSpace
  tu <- unitP <?> "TimeUnit Parser"
  skipSpace
  return (fr, fn $ num * (toMillis tu))

-- | Parse the schedule string from plain English to its corresponding data constructor
scheduleCtorP :: Parser (Frequency, (Int -> Schedule))
scheduleCtorP = do
  skipWhile ((==) '\n')
  ctorStr <- ((string "every" <?> "every") <|> (string "in" <?> "in")) <?> "Schedule ctor Parser"
  skipSpace
  case ctorStr of
    "every" -> return (Continuous, Offset)
    "in"    -> return (Once, Offset)

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
    _         -> fail "Unkown schedule token"

-- | Parse an action with its identifier
declaredActionP :: Parser (Action a)
declaredActionP = do
  _ <- string "action" <?> "Declared Action"
  skipSpace
  name <- many1 letter_ascii
  skipSpace
  bdy <- bodyP
  skipWhile ((==) '\n')
  return (Shell name bdy) -- FIXME parse other types

-- | Parse a full command body, i.e. between '{' and '}'
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
bodyRefP :: Schedule -> [Action a] -> Parser (Maybe (String, IO ()))
bodyRefP _ [] = return Nothing
bodyRefP s l = do
  skipSpace
  iden <- many1 letter_ascii
  toAction iden $ findDeclared iden
    where findDeclared n = filter (byName n) l
          byName n (Shell n' _) = n == n'
          toAction _ []    = return Nothing
          toAction i (a:_) = return $ Just (i, mkAction s a)

-- | Represent our @TimeUnit@ as an @Int@
toMillis :: TimeUnit -> Int
toMillis Seconds = 1000
toMillis Minutes = (toMillis Seconds) * 60
toMillis Hours   = (toMillis Minutes) * 60
toMillis Days    = (toMillis Hours) * 24

mkAction :: Schedule -> Action a -> IO ()
mkAction (Offset ms) (Shell _ a) = callCommand $ B.unpack a
mkAction (Offset ms) (Shell _ a) = callCommand $ B.unpack a
mkAction (Offset ms) (Shell _ a) = callCommand $ B.unpack a
