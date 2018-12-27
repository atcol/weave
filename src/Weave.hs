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
    randomSeconds,
    randomTimeBetween,
    --runSchedule,
    runPlan
    ) where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Monad          (forever, replicateM)
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (Reader, runReader)
import           Data.Bifunctor         (first)
import           Data.List              (repeat)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TI
import           Data.Time.Clock        (NominalDiffTime, UTCTime, addUTCTime,
                                         diffUTCTime, getCurrentTime)
import           GHC.Generics
import           GHC.IO.Handle          (Handle (..), hGetContents, hPutStr)
import           GHC.IO.Handle.FD       (stderr, stdin, stdout)
import           Pipes                  (Consumer (..), Pipe (..),
                                         Producer (..), await, cat, for, lift,
                                         runEffect, yield, (>->))
import qualified Pipes.Prelude          as P
import           System.Exit            (ExitCode (..))
import           System.Process         (CreateProcess (..), ProcessHandle (..),
                                         StdStream (..), createProcess_,
                                         readCreateProcessWithExitCode, shell)
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
              Shell { shName :: T.Text, actBody :: T.Text }
              -- | Nothing is declared
              | Undefined
              deriving (Eq, Show)


-- | Wraps the result of (attempting) an action
data ActionResult =
                   -- | The action succeded
                   Success T.Text
                  -- | The action did not execute
                  | Failure T.Text
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

-- | Generate seconds in the interval [0, n]
randomSeconds :: RandomGen g => g -> Int -> (NominalDiffTime, g)
randomSeconds rg mx = first realToFrac $ randomR (0, mx) rg

-- | Pick a time within the given boundaries.
randomTimeBetween :: RandomGen g => UTCTime -> UTCTime -> g -> (UTCTime, g)
randomTimeBetween s e rg = case secs of (t, ng) -> (addUTCTime t s, ng)
  where secs = randomSeconds rg (abs $ floor (diff s e))

runSchedule :: Frequency -> Schedule -> IO a -> IO [a]
runSchedule Once sc a       = next sc a >>= return . flip (:) []
runSchedule Continuous sc a = forever $ next sc a
runSchedule (N n) sc a      = replicateM n $ next sc a

-- | Execute the given plan and return its results
runPlan :: Plan -> IO ()
runPlan (Plan x)  = mapM_ statementToProcess x -- map (sequence . statementToProcess) x

-- | Prepare  @Statement@ to a list of processes and their operators
statementToProcess :: Statement -> IO ()
statementToProcess (Temporal q s []) =
  statementToProcess (Temporal q s [(Shell "inline" "", defaultOperator)]) --FIXME ensure actionless parsing works
statementToProcess (Temporal q s (x:xs)) = runSchedule q s (runEffect $ for (asProducer (fst x) >-> pipes xs) (lift . print)) >> return ()

-- | Folds the given actions into a pipe that interact according to their operator
pipes :: [(Action, Operator)] -> Pipe ActionResult ActionResult IO ()
pipes xs = foldr asPipe cat xs

-- | A folding operator for an action and the previous pipe
asPipe :: (Action, Operator) -> Pipe ActionResult ActionResult IO () -> Pipe ActionResult ActionResult IO ()
asPipe (a, opr) p = p >-> do
  o <- await
  case o of
    Success r -> do
      r <- liftIO $ pipeProcs opr r a
      yield r
    f         -> yield f

-- | Convert the @Action@ to a @Producer@
asProducer :: Action -> Producer ActionResult IO ()
asProducer (Shell n b) = do
  (c, o, e) <- liftIO $ readCreateProcessWithExitCode (shell (T.unpack b)){ std_out = CreatePipe } ""
  case c of
    ExitFailure ec -> yield $ Failure $ T.concat ["Action ", n, " failed with code ", T.pack $ show ec, T.pack e]
    ExitSuccess -> do
      yield $ Success (T.pack o)

pipeProcs :: Operator -> T.Text -> Action -> IO ActionResult
pipeProcs opr r (Shell n b) = do
  case opr of
    '|' -> do
      mp <- liftIO $ createProcess_ (T.unpack n) (shell (T.unpack b)){
                      std_out = CreatePipe, std_in = CreatePipe }
            >>= (\(i,o',_,_) -> return (i, o'))
      case mp of
        (Just ip, Just op) -> do
              liftIO $ TI.putStrLn n
              liftIO $ hPutStr ip (T.unpack r)
              liftIO $ hGetContents op >>= return . Success . T.pack
        _ -> return $ Failure $ T.concat ["Could not create stdin/stdout pipes for ", n]


