{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.Core.Consumer where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IntervalIndex (allIntervals)
import Data.Maybe (fromMaybe)
import Data.These (These (..))
import SatSim.Algo.Greedy (scheduleOn)
import SatSim.Core.BatchStreamSource (BatchStreamSource, batches)
import SatSim.Core.ScheduleRepository (ScheduleRepository (..))
import SatSim.Quantities (Seconds (..))
import SatSim.Satellite (Satellite (..))
import SatSim.Schedulable (ScheduleId (..))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import Streamly.Data.Stream.Prelude (maxThreads, parMergeBy)

data Heartbeat m = Heartbeat {unheartbeat :: m (), interval :: Seconds}

liftHeartbeat :: (MonadIO m) => Heartbeat IO -> Heartbeat m
liftHeartbeat hb@(Heartbeat {interval}) =
  Heartbeat {interval, unheartbeat = liftIO . unheartbeat $ hb}

beatForever :: (MonadIO m) => Heartbeat m -> Stream.Stream m ()
beatForever (Heartbeat {unheartbeat, interval}) =
  Stream.repeatM $ unheartbeat *> liftIO (threadDelay . floor . unSeconds . (* 1000000) $ interval)

consumeBatches ::
  ( MonadBaseControl IO m,
    BatchStreamSource m,
    ScheduleRepository m,
    MonadIO m,
    MonadThrow m
  ) =>
  Satellite ->
  Heartbeat IO ->
  m ()
consumeBatches satellite heartbeat =
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
      beat = beatForever . liftHeartbeat $ heartbeat
      beatingConsumer = parMergeBy (maxThreads 4) (const . const $ EQ) beat (Stream.mapM scheduleBatch batches)
   in Stream.fold Fold.drain beatingConsumer
