{-# LANGUAGE FlexibleInstances #-}

module SatSim.Config where

import Control.Monad.Catch (MonadCatch)
import SatSim.PubSub (RabbitMQConnectInfo)
import SatSim.Core.ScheduleRepository (ScheduleRepository, readSchedule, writeSchedule)
import Control.Monad.Trans.Reader (withReaderT, ReaderT)
import SatSim.Cache (RedisScheduleRepository(..))
import Control.Monad.IO.Class (MonadIO)
import SatSim.Core.BatchStreamSource (BatchStreamSource, batches)
import qualified SatSim.PubSub as PubSub
import qualified Streamly.Data.Stream as Stream

data ConsumerConfig = ConsumerConfig { rabbitMQConnectInfo :: RabbitMQConnectInfo, redisScheduleRepository :: RedisScheduleRepository }

instance MonadIO m => ScheduleRepository (ReaderT ConsumerConfig m) where
  writeSchedule scheduleId schedule =  withReaderT redisScheduleRepository $ writeSchedule scheduleId schedule
  readSchedule = withReaderT redisScheduleRepository . readSchedule

instance (MonadIO m, MonadCatch m) => BatchStreamSource (ReaderT ConsumerConfig m) where
  batches = Stream.morphInner (withReaderT rabbitMQConnectInfo) PubSub.consumeBatches
