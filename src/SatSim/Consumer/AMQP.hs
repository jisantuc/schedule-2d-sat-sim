{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.Consumer.AMQP where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (eitherDecode)
import Data.IntervalIndex (allIntervals)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.These (These (..))
import GHC.Float (float2Double)
import Network.AMQP
  ( Ack (..),
    ExchangeOpts (..),
    Message (..),
    QueueOpts (..),
    ackEnv,
    bindQueue,
    closeConnection,
    declareExchange,
    declareQueue,
    newExchange,
    newQueue,
    openChannel,
    openConnection,
  )
import Network.AMQP.Streamly (consume)
import SatSim.Algo.Greedy (scheduleOn)
import SatSim.Quantities (Seconds (..))
import SatSim.Satellite (Satellite (..))
import SatSim.Schedulable (Schedulable (..), ScheduleId (..))
import SatSim.ScheduleRepository (ScheduleRepository (..))
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (finallyIO)
import qualified Streamly.Data.Stream as Stream
import Streamly.Data.Stream.Prelude (maxThreads, parMergeBy)

data Heartbeat m = Heartbeat {unheartbeat :: m (), interval :: Seconds}

liftHeartbeat :: (MonadIO m) => Heartbeat IO -> Heartbeat m
liftHeartbeat hb@(Heartbeat {interval}) =
  Heartbeat {interval, unheartbeat = liftIO . unheartbeat $ hb}

beatForever :: (MonadIO m) => Heartbeat m -> Stream.Stream m ()
beatForever (Heartbeat {unheartbeat, interval}) =
  Stream.delay (float2Double . unSeconds $ interval) (Stream.repeatM unheartbeat)

consumeBatchesFromExchange ::
  (ScheduleRepository m, MonadIO m, MonadCatch m, MonadBaseControl IO m) =>
  Satellite ->
  Text ->
  Heartbeat IO ->
  m ()
consumeBatchesFromExchange satellite exchangeName' hb = do
  (conn, chan, queue) <- liftIO $ do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
    declareExchange chan (newExchange {exchangeName = exchangeName', exchangeType = "fanout"})
    (queue, _, _) <- declareQueue chan (newQueue {queueName = "", queueExclusive = True})
    bindQueue chan queue exchangeName' ""
    pure (conn, chan, queue)

  let beat = beatForever . liftHeartbeat $ hb
  let bracketed = finallyIO (closeConnection conn) (Stream.mapM callback $ consume chan queue Ack)
  Stream.fold Fold.drain $ parMergeBy (maxThreads 4) compare beat bracketed
  where
    callback (msg, envelope) =
      let scheduleNewCandidates batch = do
            schedule <- fromMaybe mempty <$> readSchedule (ScheduleId "schedule-key")
            let newSchedule = scheduleOn satellite batch schedule
            case newSchedule of
              This _ -> liftIO $ putStrLn "nothing scheduled"
              That sched -> do
                liftIO $ print ("Schedule size: " <> (show . length . allIntervals) sched)
                writeSchedule (ScheduleId "schedule-key") sched
              These errs sched -> do
                liftIO $ do
                  print ("Schedule size: " <> (show . length . allIntervals) sched)
                  print ("Num errors encountered: " <> (show . length) errs)
                writeSchedule (ScheduleId "schedule-key") sched
       in do
            either
              (liftIO . putStrLn)
              scheduleNewCandidates
              (eitherDecode (msgBody msg) :: Either String [Schedulable])
            liftIO $ ackEnv envelope
