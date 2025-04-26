{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module SatSim.ScheduleRepository where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.IntervalIndex (IntervalIndex)
import Data.Time (UTCTime)
import SatSim.Cache (RedisScheduleRepository (..))
import qualified SatSim.Cache as Cache
import SatSim.Satellite (ScheduleId)
import SatSim.Schedulable (Scheduled)

type Schedule = IntervalIndex UTCTime Scheduled

class ScheduleRepository m where
  readSchedule :: ScheduleId -> m (Maybe Schedule)
  writeSchedule :: ScheduleId -> Schedule -> m ()

instance (MonadIO m) => ScheduleRepository (ReaderT RedisScheduleRepository m) where
  readSchedule scheduleId =
    ask
      >>= (\(RedisScheduleRepository {conn}) -> liftIO $ Cache.readSchedule conn scheduleId)
  writeSchedule scheduleId schedule =
    ask
      >>= ( \(RedisScheduleRepository {conn}) ->
              liftIO $ Cache.writeSchedule conn scheduleId schedule
          )
