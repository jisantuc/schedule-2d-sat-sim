{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.Consumer.AMQP where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecode)
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
import Streamly.Data.Stream (finallyIO)
import qualified Streamly.Data.Stream as Stream

data Heartbeat m = Heartbeat {unheartbeat :: m (), interval :: Seconds}

beatForever :: (MonadIO m) => Heartbeat m -> Stream.Stream m ()
beatForever (Heartbeat {unheartbeat, interval}) =
  Stream.repeatM (unheartbeat *> liftIO (threadDelay (floor interval * 1000000)))

consumeBatchesFromExchange ::
  (ScheduleRepository m, MonadIO m, MonadCatch m) =>
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

  let beat = beatForever hb
  let consumer = finallyIO (closeConnection conn) (Stream.mapM callback (consume chan queue Ack))
  liftIO $ print "oh no"
  where
    callback :: (ScheduleRepository m, MonadIO m) => (Message, Envelope) -> m ()
    callback (msg, envelope) =
      let scheduleNewCandidates batch = do
            schedule <- fromMaybe mempty <$> readSchedule (ScheduleId "schedule-key")
            let newSchedule = scheduleOn satellite batch schedule
            case newSchedule of
              This _ -> liftIO $ do
                putStrLn "nothing scheduled"
              That sched -> do
                writeSchedule (ScheduleId "schedule-key") sched
              These _ sched -> do
                writeSchedule (ScheduleId "schedule-key") sched
       in do
            either
              (liftIO . putStrLn)
              scheduleNewCandidates
              (eitherDecode (msgBody msg) :: Either String [Schedulable])
            liftIO $ ackEnv envelope
