{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.Consumer.AMQP where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecode)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.These (These (..))
import Network.AMQP
  ( Ack (..),
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
import SatSim.Quantities (Seconds (..))
import SatSim.Satellite (Satellite (..))
import SatSim.Schedulable (Schedulable (..))
import SatSim.ScheduleRepository (ScheduleId (ScheduleId), ScheduleRepository (..))

data Heartbeat m = Heartbeat {unheartbeat :: m (), interval :: Seconds}

beatForever :: (MonadIO m) => Heartbeat m -> m ()
beatForever (Heartbeat {unheartbeat, interval}) =
  void . forever $
    liftIO (threadDelay (floor interval * 1000000)) *> unheartbeat

consumeBatchesFromExchange :: (ScheduleRepository a) => Satellite -> a -> Text -> Heartbeat IO -> IO ()
consumeBatchesFromExchange satellite repository exchangeName' hb = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn
  declareExchange chan (newExchange {exchangeName = exchangeName', exchangeType = "fanout"})
  (queue, _, _) <- declareQueue chan (newQueue {queueName = "", queueExclusive = True})
  bindQueue chan queue exchangeName' ""
  void $ consumeMsgs chan queue Ack callback

  beatForever hb

  closeConnection conn
  where
    callback :: (Message, Envelope) -> IO ()
    callback (msg, envelope) = do
      case eitherDecode (msgBody msg) of
        Right batch -> do
          schedule <- fromMaybe mempty <$> readSchedule repository (ScheduleId "schedule-key")
          let newSchedule = scheduleOn satellite batch schedule
          case newSchedule of
            This _ -> putStrLn "nothing scheduled"
            That sched -> writeSchedule repository (ScheduleId "schedule-key") sched
            These _ sched -> writeSchedule repository (ScheduleId "schedule-key") sched
        Left e -> putStrLn e
      either putStrLn (print . length) (eitherDecode (msgBody msg) :: Either String [Schedulable])
      ackEnv envelope
