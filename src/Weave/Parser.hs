{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | The Weave parsing API
module Weave.Parser (
  TimeUnit (..),
  ParseResult (..),

  -- | Parsers
  parsePlan,
  temporalP,
  unitP,

  -- | Functions
  toMillis,
  supportedUnits
  ) where

import           Control.Applicative     ((<|>))
import           Data.Attoparsec.Text
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import           Prelude                 (error, fail, read)
import           Protolude               hiding (option, takeWhile)
import           Weave

-- | All possible outcomes of an actoin reference parse
data ActionRefParseResult = -- | The action reference was not found
                            ActionNotFound T.Text
                            -- | The identified action reference
                            | ActionRef T.Text Action
                            deriving (Show)

-- | The unit of time supported as Temporal Expressions
data TimeUnit = Seconds
              | Minutes
              | Hours
              | Days
              deriving (Enum, Eq, Show)

data ParseResult = Success Plan
                 | MalformedPlan T.Text

supportedUnits = ['s', 'm', 'h', 'd']

-- | Parse the entire Plan from the given string
parsePlan :: T.Text -> ParseResult
parsePlan = wrap . parseOnly planP
  where wrap (Left s)  = MalformedPlan $ T.concat ["Parse error: ", T.pack s]
        wrap (Right p) = Success p

-- | The entire document parser
planP :: Parser Plan
planP = do
  acts <- many' actionBlockP
  stmts <- many' $ statementP acts
  return $ Plan stmts

statementP :: [Action] -> Parser Statement
statementP acts = do
  (fr, s) <- temporalP
  Temporal fr s <$> inlineOrReferenceP acts

-- | Parse an inline body or action reference expression
inlineOrReferenceP :: [Action] -> Parser [(Action, Operator)]
inlineOrReferenceP acts = do
  r <- option Undefined inlineBodyP
  case r of
    Undefined -> actionExpressionsP acts
    shell     -> return [(shell, defaultOperator)]

-- | Parse the inline action declaration
inlineBodyP :: Parser Action
inlineBodyP = Action Shell "inline" <$> bodyP

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
  le <- letter <?> "Unit ctor"
  skipSpace
  ct le
  where ct 's' = return Seconds
        ct 'm' = return Minutes
        ct 'h' = return Hours
        ct 'd' = return Days
        ct t   = fail $ "Unknown schedule token: " ++ show t

-- | Parse an action block
actionBlockP :: Parser Action
actionBlockP = do
  blkType <- (many1 letter <?> "Action type")
  skipSpace
  name <- many1 letter >>= return . T.pack
  skipSpace
  bdy <- bodyP
  skipWhile ((==) '\n')
  con (read $ T.unpack $ T.toTitle $ T.pack blkType) name bdy
    where con t n b = return $ Action t n b

-- | Parse a full body, i.e. between '{' and '}'
bodyP :: Parser T.Text
bodyP = do
  op <- peekChar'
  skipSpace

  res <- (char op) *> manyTill anyChar (inverse op) <?> "Body"

  return $ T.pack res
    where inverse '{' = char '}' *> endOfLine *> endOfLine
          inverse '@' = endOfLine
          inverse ':' = endOfLine
          inverse c   = fail $ "Unknown body enclosing character: " ++ show c

-- | Parse many action expressions
actionExpressionsP :: [Action] -> Parser [(Action, Operator)]
actionExpressionsP l = many1 $ actionExpressionP l

-- | Parse the body reference and an operator on its RHS
actionExpressionP :: [Action] -> Parser (Action, Operator)
actionExpressionP l = do
  ref <- actionReferenceP l
  c <- option defaultOperator operatorsP
  f ref c
    where f (ActionRef _ a) c    = return (a, c)
          f (ActionNotFound i) c = return (Action Shell i i, c)

-- | Parse a supported operator
operatorsP :: Parser Operator
operatorsP = do
  skipSpace
  c <- char '|' <|> char '&' <|> char ','
  skipSpace
  case c of
    '|' -> return Pipe
    '&' -> return And
    ',' -> return Sequence

-- | Parse one body reference (e.g. @action1@ in @action1 | action2@) to an action
-- and find its action in the given list
actionReferenceP :: [Action] -> Parser ActionRefParseResult
actionReferenceP l = do
  skipSpace
  iden <- T.pack <$> many1 letter
  summarise iden (findDeclared iden)
    where findDeclared n = filter (byName n) l
          byName n (Action _ n' _) = n == n'
          byName _ Undefined       = False
          summarise i []    = return $ ActionNotFound i
          summarise i (x:_) = return $ ActionRef i x -- only take the first

-- | Represent our @TimeUnit@ as an @Int@
toMillis :: TimeUnit -> Int
toMillis Seconds = 1000
toMillis Minutes = (toMillis Seconds) * 60
toMillis Hours   = (toMillis Minutes) * 60
toMillis Days    = (toMillis Hours) * 24
