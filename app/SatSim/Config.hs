{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SatSim.Config where

import Control.Monad.Catch (MonadCatch)
import SatSim.PubSub (RabbitMQConnectInfo)
import SatSim.Core.ScheduleRepository (ScheduleRepository, readSchedule, writeSchedule)
import Control.Monad.Trans.Reader (withReaderT, ReaderT)
import SatSim.Cache (RedisScheduleRepository(..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import SatSim.Core.BatchStreamSource (BatchStreamSource, batches)
import qualified SatSim.PubSub as PubSub
import qualified Streamly.Data.Stream as Stream
import Colog (HasLog, getLogAction, setLogAction, LogAction (..))

data ConsumerConfig = ConsumerConfig { rabbitMQConnectInfo :: RabbitMQConnectInfo, redisScheduleRepository :: RedisScheduleRepository }

instance (MonadIO m) => HasLog ConsumerConfig msg m where
  -- TODO: do I have useful context here? I don't think so. Maybe the time?
  -- some info that the log is definitely from the consumer? I don't know.
  getLogAction _ = LogAction (\_ -> liftIO $ putStr "oh no")
  setLogAction _ conf = conf

instance MonadIO m => ScheduleRepository (ReaderT ConsumerConfig m) where
  writeSchedule scheduleId schedule =  withReaderT redisScheduleRepository $ writeSchedule scheduleId schedule
  readSchedule = withReaderT redisScheduleRepository . readSchedule

instance (MonadIO m, MonadCatch m) => BatchStreamSource (ReaderT ConsumerConfig m) where
  batches = Stream.morphInner (withReaderT rabbitMQConnectInfo) PubSub.consumeBatches
