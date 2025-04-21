{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.Consumer.AMQP where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Aeson (eitherDecode)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.These (These (..))
import Network.AMQP
  ( Ack (..),
    Connection,
    Envelope,
    ExchangeOpts (..),
    Message (..),
    QueueOpts (..),
    ackEnv,
    bindQueue,
    closeConnection,
    consumeMsgs,
    declareExchange,
    declareQueue,
    newExchange,
    newQueue,
    openChannel,
    openConnection,
  )
import SatSim.Algo.Greedy (scheduleOn)
import SatSim.Cache (RedisScheduleRepository (RedisScheduleRepository))
import SatSim.Quantities (Seconds (..))
import SatSim.Satellite (Satellite (..))
import SatSim.Schedulable (Schedulable (..), ScheduleId (..))
import SatSim.ScheduleRepository (ScheduleRepository (..))

data Heartbeat m = Heartbeat {unheartbeat :: m (), interval :: Seconds}

beatForever :: (MonadIO m) => Heartbeat m -> m ()
beatForever (Heartbeat {unheartbeat, interval}) =
  void . forever $
    unheartbeat *> liftIO (threadDelay (floor interval * 1000000))

consumeBatchesFromExchange ::
  (ScheduleRepository m, MonadIO m) =>
  Satellite ->
  Text ->
  Heartbeat m ->
  ReaderT RedisScheduleRepository m ()
consumeBatchesFromExchange satellite exchangeName' hb = do
  liftIO $ do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
    declareExchange chan (newExchange {exchangeName = exchangeName', exchangeType = "fanout"})
    (queue, _, _) <- declareQueue chan (newQueue {queueName = "", queueExclusive = True})
    bindQueue chan queue exchangeName' ""
    -- Stuck! callback has to be -> IO (), but I want to work in `m`, and `consumeMsg` fixes the effect type to `IO`
    void $ consumeMsgs chan queue Ack callback

    beatForever hb

    closeConnection conn
  where
    callback :: (ScheduleRepository m, MonadIO m) => (Message, Envelope) -> m ()
    callback (msg, envelope) = do
      case eitherDecode (msgBody msg) of
        Right batch -> do
          schedule <- fromMaybe mempty <$> readSchedule (ScheduleId "schedule-key")
          let newSchedule = scheduleOn satellite batch schedule
          case newSchedule of
            This _ -> liftIO $ putStrLn "nothing scheduled"
            That sched -> writeSchedule (ScheduleId "schedule-key") sched
            These _ sched -> writeSchedule (ScheduleId "schedule-key") sched
        Left e -> liftIO $ putStrLn e
      either (liftIO . putStrLn) (liftIO . print . length) (eitherDecode (msgBody msg) :: Either String [Schedulable])
      liftIO $ ackEnv envelope
