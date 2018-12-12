{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | The Chaos parsing API
module Data.Time.Schedule.Chaos.Parser (
  Plan (..),
  TimeUnit (..),

  parsePlan,
  temporalP,
  toMillis
  ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B
import           Data.Time.Schedule.Chaos         (Cause (..), Frequency (..),
                                                   Plan (..), Schedule (..))
import           Debug.Trace                      (traceM)
import           Prelude                          hiding (takeWhile)
import           System.Process                   (callCommand)

-- | All possible outcomes of an actoin reference parse
data ActionRefParseResult =
                          -- | The action reference was not found
                          ActionNotFound String
                          -- | The identified action reference
                          | ActionRef String Action
                          deriving (Show)

-- | The unit of time supported as Temporal Expressions
data TimeUnit = Seconds
              | Minutes
              | Hours
              | Days
              deriving (Enum, Eq, Show)

-- | A wrapper for actions/behaviour
data Action = -- | A basic shell command
              Shell String B.ByteString
              -- | Nothing is declared
              | Undefined
              deriving (Eq, Show)

-- | Parse the entire Plan from the given string
parsePlan :: B.ByteString -> Either String (Plan ())
parsePlan = wrap . parseOnly planP
  where wrap (Left e) = Left $ "Parse error: " ++ show e -- For testing
        wrap r        = r

-- | The entire document parser
planP :: Parser (Plan ())
planP = many' actionBlockP >>= scheduleP

-- | Parse the temporal and action reference section
scheduleP :: [Action] -> Parser (Plan ())
scheduleP l = do
  (fr, s) <- temporalP
  r <- option Undefined inlineBodyP
  case r of
    Undefined -> actionExpressionsP l >>= return . Plan fr s . sequence_ . map (actionToIO s . fst)
    shell -> return $ Plan fr s (actionToIO s shell)

-- | Parse many action expressions
actionExpressionsP :: [Action] -> Parser [(Action, B.ByteString)]
actionExpressionsP l = many1 (actionExpressionP l)

-- | Parse the inline action declaration
inlineBodyP :: Parser Action
inlineBodyP = Shell "inline" <$> bodyP

-- | Parse a frequency and schedule
temporalP :: Parser (Frequency, Schedule)
temporalP = do
  (fr, fn) <- scheduleCtorP <?> "Schedule Constructor"
  num <- round <$> double
  skipSpace
  tu <- unitP <?> "TimeUnit"
  skipSpace
  return (fr, fn $ num * (toMillis tu))

-- | Parse the schedule string from plain English to its corresponding data constructor
scheduleCtorP :: Parser (Frequency, (Int -> Schedule))
scheduleCtorP = do
  skipWhile ((==) '\n')
  ctorStr <- ((string "every" <?> "every") <|> (string "in" <?> "in")) <?> "Schedule Ctor"
  skipSpace
  case ctorStr of
    "every" -> return (Continuous, Offset)
    "in"    -> return (Once, Offset)
    s       -> error $ "Unknown frequency: " ++ show s

-- | Parse a @TimeUnit@ from plain English
unitP :: Parser TimeUnit
unitP = do
  ctorStr <- (string "seconds"
              <|> string "minutes"
              <|> string "hours"
              <|> string "days") <?> "Unit ctor"
  skipSpace
  case ctorStr of
    "seconds" -> return Seconds
    "minutes" -> return Minutes
    "hours"   -> return Hours
    "days"    -> return Days
    t         -> error $ "Unkown schedule token: " ++ show t

-- | Parse an action block
actionBlockP :: Parser Action
actionBlockP = do
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
  res <- takeWhile (/= (inverse op)) <?> "Body contents"
  skipMany (char $ inverse op)
  return res
    where inverse '{' = '}'
          inverse '@' = '\n'
          inverse ':' = '\n'

-- | Parse the body reference and an operator on its RHS
actionExpressionP :: [Action] -> Parser (Action, B.ByteString)
actionExpressionP l = do
  ref <- actionReferenceP l
  c <- option defaultOperator operatorsP
  f ref $ B.pack $ show c
    where f (ActionRef _ a) c    = return (a, c)
          f (ActionNotFound i) c = return (Shell i "", c)
          defaultOperator = ' '

-- | Parse a supported operator
operatorsP :: Parser Char
operatorsP = skipSpace >> char '|' <|> char '&' <|> char ',' <|> char '¬'

-- | Parse one body reference (e.g. @action1@ in @action1 | action2@) to an action
-- and find its action in the given list
actionReferenceP :: [Action] -> Parser ActionRefParseResult
actionReferenceP l = do
  skipSpace
  iden <- many1 letter_ascii
  summarise iden (findDeclared iden)
    where findDeclared n = filter (byName n) l
          byName n (Shell n' _) = n == n'
          summarise i [] = return $ ActionNotFound i
          summarise i x  = return $ ActionRef i $ head x -- only take the first

-- | Represent our @TimeUnit@ as an @Int@
toMillis :: TimeUnit -> Int
toMillis Seconds = 1000
toMillis Minutes = (toMillis Seconds) * 60
toMillis Hours   = (toMillis Minutes) * 60
toMillis Days    = (toMillis Hours) * 24

actionToIO :: Schedule -> Action -> IO ()
actionToIO (Offset ms)  (Shell _ a) = next ms $ callCommand $ B.unpack a
actionToIO (Instant t) (Shell _ a)  = next t $ callCommand $ B.unpack a
actionToIO (Window s e) (Shell _ a) = next (s, e) $ callCommand $ B.unpack a
