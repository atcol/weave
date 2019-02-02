{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wall #-}

{-| The raw type definitions for the Weave API.

-}
module Weave.Types (
    -- | Typeclasses
    Evented (..),

    -- | Constructors
    Action (..),
    ActionType (..),
    ActionResult (..),
    Frequency (..),
    Operator (..),
    Plan (..),
    Schedule (..),
    Statement (..),
    ServiceDescriptor (..),
    Weave (..),

    defaultOperator,

  ) where

import           Data.Aeson          (FromJSON (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)
import           GHC.Generics
import           Protolude           hiding (diff, for)
import           System.Random       (Random (..), RandomGen, randomR)

-- | An operator for deciding what to do with action results, where:
--  - , means "ignore"
--  - | is "pipe", just like a unix pipe
--  - & is logical AND
--  - || is logical OR
data Operator = Sequence | Pipe | And | Or deriving (Show, Eq)

-- | A wrapper for various action blocks/bodies
data ActionType = Shell | Service deriving (Eq, Show, Read)

-- | A wrapper for actions/behaviour
data Action = Action { actType :: ActionType, actName :: T.Text, actBody :: T.Text }
              -- | Nothing is declared
              | Undefined
              deriving (Eq, Show, Generic)


-- | Wraps the result of (attempting) an action
data ActionResult = -- | The action succeded
                    Success T.Text
                    -- | The action did not execute
                    | Failure T.Text
                    deriving (Eq, Show)

-- | A payload for parsed service bodies
data ServiceDescriptor = ServiceDescriptor { url :: Text, method :: Maybe Text, headers :: HashMap Text Text, body :: Maybe Text }
  deriving (Show, Eq, Generic)

instance FromJSON ServiceDescriptor

-- | A descriptor of a cause and some associated action expressions
data Statement = Temporal Frequency Schedule [(Action, Operator)]
  deriving (Eq, Show)

-- | An execution plan with a trigger type (@Statement@)
data Plan = Plan [Statement]
  deriving (Show)
--
-- | The default operator if one isn't supplied
defaultOperator :: Operator
defaultOperator = Sequence

-- | An temporal event
data Schedule =
  -- | A point in the future, in ms
  Offset Int
  -- | A point in the future, as a @UTCTime@
  | Instant UTCTime
  -- | A lower & upper time boundary
  | Window UTCTime UTCTime
  deriving (Read, Show, Eq, Generic)

-- | The amount of times of some "thing", e.g. action, schedule
data Frequency = Once | Continuous | N Int deriving (Eq, Read, Show)

-- | Operations for sourcing events
class (MonadIO m) => Evented m s where

  -- | Request the next event, where @s@ is the event source descriptor,
  -- and @m b@ is the event generation action
  next :: s -> m b -> m b

  -- | Invoke @next@ @Int@ times
  times :: Int -> s -> m b -> m [b]
  times n s a = replicateM n (next s a)

  -- | Generate events according to @s@ at most @[1, n]@ times, using the
  -- supplied random generator
  atMost :: RandomGen g => Int -> s -> m b -> g -> m (g, [b])
  atMost n s a g = return (randomR (1, n) g) >>= (\(n', g') -> do
    v <- times n' s a
    return (g', v))

  -- | Generate an event, using @s@ as the *lower* bound
  lower :: s -> m b -> m b

  -- | Generate an event, using @s@ as the *upper* bound
  upper :: s -> m b -> m b

-- | Run @Action@s with input
class Weave a i o where
  actOn :: a -> Maybe i -> IO o
