{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Core.ScheduleRepository where

import Control.Concurrent (modifyMVar_, takeMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Functor ((<&>))
import Data.IntervalIndex (IntervalIndex)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import SatSim.Cache (LocalScheduleRepository (..), RedisScheduleRepository (..))
import qualified SatSim.Cache as Cache
import SatSim.Schedulable (ScheduleId, Scheduled)

type Schedule = IntervalIndex UTCTime Scheduled

class ScheduleRepository m where
  readSchedule :: ScheduleId -> m (Maybe Schedule)
  writeSchedule :: ScheduleId -> Schedule -> m ()

instance (MonadIO m) => ScheduleRepository (ReaderT RedisScheduleRepository m) where
  readSchedule scheduleId =
    ask
      >>= (\redisRepository -> liftIO $ Cache.readSchedule redisRepository scheduleId)
  writeSchedule scheduleId schedule =
    ask
      >>= ( \redisRepository ->
              liftIO $ Cache.writeSchedule redisRepository scheduleId schedule
          )

instance (MonadIO m) => ScheduleRepository (ReaderT LocalScheduleRepository m) where
  readSchedule scheduleId =
    ask >>= \(LocalScheduleRepository {unMVar}) -> liftIO $ takeMVar unMVar <&> Map.lookup scheduleId
  writeSchedule scheduleId schedule =
    ask >>= (\(LocalScheduleRepository {unMVar}) -> liftIO $ modifyMVar_ unMVar (pure . Map.insert scheduleId schedule))
