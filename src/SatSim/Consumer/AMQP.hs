{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.Consumer.AMQP where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (eitherDecode)
import Data.IntervalIndex (allIntervals)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.These (These (..))
import Network.AMQP
  ( Ack (..),
    Channel,
    Message (..),
    ackEnv,
  )
import Network.AMQP.Streamly (consume)
import SatSim.Algo.Greedy (scheduleOn)
import SatSim.Quantities (Seconds (..))
import SatSim.Satellite (Satellite (..))
import SatSim.Schedulable (Schedulable (..), ScheduleId (..))
import SatSim.Core.ScheduleRepository (ScheduleRepository (..))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import Streamly.Data.Stream.Prelude (maxThreads, parMergeBy)
import SatSim.Core.BatchStreamSource (BatchStreamSource, batches)

data Heartbeat m = Heartbeat {unheartbeat :: m (), interval :: Seconds}

liftHeartbeat :: (MonadIO m) => Heartbeat IO -> Heartbeat m
liftHeartbeat hb@(Heartbeat {interval}) =
  Heartbeat {interval, unheartbeat = liftIO . unheartbeat $ hb}

beatForever :: (MonadIO m) => Heartbeat m -> Stream.Stream m ()
beatForever (Heartbeat {unheartbeat, interval}) =
  Stream.repeatM $ unheartbeat *> liftIO (threadDelay . floor . unSeconds . (* 1000000) $ interval)

-- build from:
-- made it to the last step 😎
-- consume + schedule + add the hb back
consumeBatches :: (BatchStreamSource m, ScheduleRepository m, MonadIO m) =>
  Satellite -> m ()
consumeBatches satellite =
  let scheduleBatch batch = do
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
   in Stream.fold Fold.drain $ Stream.mapM scheduleBatch batches

-- TODO: BatchEventSource m? With instance for ReaderT (Channel, Text) m a?
-- what's the API goal here? I don't want to know anything about m
-- I _do_ want to be able to get a stream of [Schedulable] given a mystery m
-- I don't want Channel/Text as argument types
-- do I want ReaderT m (chan, queue) as the instance, and consumeBatches as bracketing
--   that setup around consume?
consumeBatchesFromExchange ::
  (BatchStreamSource m, ScheduleRepository m, MonadIO m, MonadCatch m, MonadBaseControl IO m) =>
  Satellite ->
  Channel ->
  Text ->
  Heartbeat IO ->
  m ()
consumeBatchesFromExchange satellite chan queue hb = do
  let beat = beatForever . liftHeartbeat $ hb
  let bracketed = Stream.mapM callback $ consume chan queue Ack
  let heartbeatingConsumer = parMergeBy (maxThreads 4) (const . const $ EQ) beat bracketed
  Stream.fold Fold.drain batches
  where
    callback (msg, envelope) =
      let scheduleNewCandidates batch =
            liftIO (print (show . length $ batch)) *> do
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
