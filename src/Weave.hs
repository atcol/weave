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

    -- | Types
    Action (..),
    ActionType (..),
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

import           Control.Concurrent      (threadDelay)
import           Control.Monad           (forever, replicateM)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (eitherDecode)
import           Data.Bifunctor          (first)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import qualified Data.Text.IO            as TI
import           Data.Time.Clock         (NominalDiffTime, UTCTime, addUTCTime,
                                          diffUTCTime, getCurrentTime)
import           GHC.Generics
import           GHC.IO.Handle           (Handle, hGetContents)
import           GHC.IO.Handle.FD        (stdin, stdout)
import           Pipes                   (Consumer, Pipe, Producer, await, cat,
                                          for, lift, runEffect, yield, (>->))
import qualified Pipes.Prelude           as P
import           Prelude                 (error, id)
import           Protolude               hiding (diff, for)
import           System.Exit             (ExitCode (..))
import           System.Process          (CreateProcess (..),
                                          ProcessHandle (..), StdStream (..),
                                          createProcess_,
                                          readCreateProcessWithExitCode, shell)
import           System.Random           (Random (..), RandomGen, newStdGen,
                                          randomR)
import           Weave.Network.HTTP
import           Weave.Types

-- | For brevity - the result of an action and the operator to apply
type ActResOpPair = (ActionResult, Operator)

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
  statementToProcess (Temporal q s [(Action Shell "inline" "", defaultOperator)]) --FIXME ensure actionless parsing works
statementToProcess (Temporal q s (x:xs)) = runSchedule q s (runEffect $ for (asProducer x >-> pipes xs) (rawPrint . fst)) >> return ()
  where rawPrint (Success r) = lift $ putStrLn $ T.unpack r
        rawPrint (Failure r) = lift $ putStrLn $ "Error:" ++ T.unpack r

-- | Folds the given actions into a pipe that interact according to their operator
pipes :: [(Action, Operator)] -> Pipe ActResOpPair ActResOpPair IO ()
pipes xs = foldr asPipe cat xs

-- | A folding operator for an action and the previous pipe
asPipe :: (Action, Operator) -> Pipe ActResOpPair ActResOpPair IO () -> Pipe ActResOpPair ActResOpPair IO ()
asPipe (a, _) p = p >-> do
  o <- await
  re <- liftIO $ handleResult o
  yield re
    where handleResult (Success r, opr) = handleAct opr r a >>= \o' -> return (Success o', opr)
          handleResult r                 = return r

-- | Convert the @Action@ to a @Producer@
asProducer :: (Action, Operator) -> Producer ActResOpPair IO ()
asProducer ((Action Shell n b), op) = do
  (c, o, e) <- liftIO $ readCreateProcessWithExitCode (shell (T.unpack b)){ std_out = CreatePipe } ""
  case c of
    ExitFailure ec -> yield (Failure $ T.concat ["Action ", n, " failed with code: ", T.pack $ show ec, T.pack e], op)
    ExitSuccess -> yield (Success (T.pack o), op)
asProducer ((Action Service n b), op) = do
  r <- liftIO (runService b Nothing)
  yield (Success r, op)

-- | Handle the next action, given the previous operator and the result of the last action
handleAct :: Operator -> T.Text -> Action -> IO T.Text
handleAct '|' r (Action Service _ b) = runService b $ Just r

handleAct ',' _ (Action Shell n b) = do
  (Just op) <- liftIO $ createProcess_ (T.unpack n) (shell (T.unpack b)){ std_out = CreatePipe }
        >>= (\(_,o,_,_) -> return o)
  hGetContents op >>= return . T.pack
handleAct '|' r (Action Shell n b) = do
  (Just ip, Just op) <- createProcess_ (T.unpack n) (shell (T.unpack b)){
                          std_out = CreatePipe, std_in = CreatePipe }
                >>= (\(i,o',_,_) -> return (i, o'))
  hPutStr ip (T.unpack r)
  hGetContents op >>= return . T.pack
handleAct o _ (Action _ n _) = error $ "Unsupported operator " ++ show o ++ " for action " ++ T.unpack n

-- | Parse the service descriptor & run it with the given input
runService :: T.Text -> Maybe T.Text -> IO T.Text
runService b i = do
  -- Delay JSON parsing so we can use templates later
  case eitherDecode (cs $ "{" ++ (cs b) ++ "}") of
    Left e                           -> error $ "Invalid service body: " ++ e
    Right (ServiceDescriptor u m h mb)  -> http u (fromMaybe "GET" m) h (head $ catMaybes [i, mb, Just ""])
