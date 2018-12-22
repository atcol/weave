{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wall #-}

-- | The core Weave API
module Weave
  (
    -- | Typeclasses
    Weave (..),

    -- | Data constructors
    Action (..),
    Frequency (..),
    Operator,
    Plan (..),
    Schedule (..),
    Statement (..),

    -- | Functions
    defaultOperator,
    genTime,
    mkOffsets,
    mkSchedules,
    randomSeconds,
    randomTimeBetween,
    --runSchedule,
    runPlan
    ) where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Monad          (forever, replicateM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (Reader, runReader)
import           Data.Bifunctor         (first)
import           Data.List              (repeat)
import qualified Data.Text              as T
import           Data.Time.Clock        (NominalDiffTime, UTCTime, addUTCTime,
                                         diffUTCTime, getCurrentTime)
import           GHC.Generics
import           GHC.IO.Handle          (Handle (..), hGetContents, hPutStr)
import           GHC.IO.Handle.FD       (stderr, stdin, stdout)
import           System.Process         (ProcessHandle (..),
                                         runInteractiveCommand, spawnCommand)
import           System.Random          (Random (..), RandomGen, newStdGen,
                                         randomR)

-- | An operator for deciding what to do with action results, where:
--  - , means "ignore"
--  - | is "pipe", just like a unix pipe
--  - & is logical AND
--  - || is logical OR
type Operator = Char

-- | A wrapper for actions/behaviour
data Action = -- | A basic shell command
              Shell { actName :: T.Text, actBody :: T.Text }
              -- | Nothing is declared
              | Undefined
              deriving (Eq, Show)

-- | A wrapper for a process and its in, out and error handles
data Process = Process Handle Handle Handle ProcessHandle

-- | A descriptor of a cause and some associated action expressions
data Statement = Temporal Frequency Schedule [(Action, Char)]
  deriving (Eq, Show)

-- | An execution plan with a trigger type (@Statement@)
data Plan = Plan [Statement]
  deriving (Show)
--
-- | The default operator if none is supplied
defaultOperator :: Operator
defaultOperator = ','


-- | An event source descriptor based on time
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
class (MonadIO m) => Weave m s where

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

instance Weave IO Int where
  next = lower

  -- | A delay of @ms@ milliseconds before executing @a@
  lower ms a = threadDelay (ms * 1000) >> a

  --upper ms a = newStdGen >>= genTime (Offset ms) >>=

instance Weave IO UTCTime where
  -- | A delay of @t - getCurrentTime@  before executing @a@
  next t a = getCurrentTime >>= return . timeDiffSecs >>= (\t' -> next t' a)
    where timeDiffSecs :: UTCTime -> Int
          timeDiffSecs = round . flip diffUTCTime t

instance Weave IO (UTCTime, UTCTime) where
  next (s, e) a = newStdGen >>= return . randomTimeBetween s e >>= flip next a . fst

-- | Generate events, parameterised by time
instance Weave IO Schedule where

  next (Offset ms) a  = next ms a
  next (Window s e) a = newStdGen >>=
    return . randomTimeBetween s e >>=
    return . fst >>=
    (\t -> next t a)

-- | Randomly pick a time compatible with the given schedule
genTime :: (MonadIO m, RandomGen g) => Schedule -> g -> m (UTCTime, g)
genTime (Offset ms) rg = do
  now <- liftIO $ getCurrentTime
  let end' = addUTCTime (realToFrac (ms `div` 1000)) now
  genTime (Window now end') rg
genTime (Window s e) rg = return $ randomTimeBetween s e rg

-- | Shorthand for the difference between two UTCTime instances
diff :: UTCTime -> UTCTime -> NominalDiffTime
diff = diffUTCTime

-- | Delay for a random amount within the schedule
delayFor :: RandomGen g => Schedule -> g -> IO g
delayFor sc g = do
  let (ti, g') = undefined -- <- genTime sc g
  del <- case sc of
              Offset _   -> getCurrentTime >>= return . flip getDelay ti
  threadDelay del
  return g'

-- | Turn the @UTCTime@ to its microseconds
getDelay :: UTCTime -> UTCTime -> Int
getDelay s t = delay
  where tDiff = (round $ diff s t) :: Int
        micros = 1000 * 1000
        delay = abs $ tDiff * micros

-- | Construct an infinite list of constant @Offset@ instances
mkOffsets :: Int -> [Schedule]
mkOffsets n = Offset <$> repeat n

-- | Construct the schedule using the supplied reader
mkSchedules :: Reader e Schedule -> e -> [IO a] -> [(Schedule, IO a)]
mkSchedules r e acts = map (\ac -> (runReader r e, ac)) acts

-- | Generate seconds in the interval [0, n]
randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg mx = first realToFrac $ randomR (0, mx) rg

-- | Pick a time within the given boundaries.
randomTimeBetween :: RandomGen g => UTCTime -> UTCTime -> g -> (UTCTime, g)
randomTimeBetween s e rg = case secs of (t, ng) -> (addUTCTime t s, ng)
  where secs = randomSeconds rg (abs $ floor (diff s e))

--runSchedule :: Frequency -> (Schedule, IO a) -> IO [a]
--runSchedule Once (sc, a)         = next sc a >>= return . flip (:) []
--runSchedule (Continuous) (sc, a) = forever $ next sc a
--runSchedule (N n) (sc, a)        = replicateM n $ next sc a

-- | Execute the given plan and return its results
runPlan :: Plan -> IO ()
runPlan (Plan []) = error "No actions or schedules defined"
runPlan (Plan x)  = undefined -- map (sequence . statementToProcess) x

-- | Prepare  @Statement@ to a list of processes and their operators
statementToProcess :: Statement -> [(IO Process, Operator)]
statementToProcess (Temporal q s []) =
  statementToProcess (Temporal q s [(Shell "inline" "", defaultOperator)]) --FIXME ensure actionless parsing works
statementToProcess (Temporal q s x) = undefined

actionToProcess :: Schedule -> Action -> IO Process
actionToProcess (Offset m)   (Shell _ a) = next m $ run a
actionToProcess (Instant t)  (Shell _ a) = next t $ run a
actionToProcess (Window s e) (Shell _ a) = next (s, e) $ run a

-- | Take the batch command and create a process from it
run :: T.Text -> IO Process
run b = do
  c <- runInteractiveCommand (T.unpack b)
  return $ toProc c
    where toProc (i, o, e, ph) = Process i o e ph

-- | Chain the two process by their @stdout@ and @stdin@, respectively
chain :: Process -> Process -> IO ()
chain (Process i _ _ _) (Process _ o _ _) = hGetContents i >>= forkIO . hPutStr o >> return ()
