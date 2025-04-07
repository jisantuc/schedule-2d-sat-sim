{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Cache where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString (toStrict)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Functor (void)
import Data.IntervalIndex (IntervalIndex)
import qualified Data.IntervalIndex as IntervalIndex
import Data.Time (UTCTime)
import Database.Redis (ConnectInfo, Connection, checkedConnect, defaultConnectInfo, get, runRedis, set)
import SatSim.Schedulable (Scheduled)
import SatSim.ScheduleRepository (ScheduleId (..), ScheduleRepository (..))

intervalIndexFromByteString :: BS.ByteString -> Maybe (IntervalIndex UTCTime Scheduled)
intervalIndexFromByteString bs = case eitherDecodeStrict' bs of
  Right v -> Just $ IntervalIndex.fromList v
  Left _ -> Nothing

scheduleIdKey :: ScheduleId -> BS.ByteString
scheduleIdKey (ScheduleId i) = pack i

readSchedule :: Connection -> ScheduleId -> IO (Maybe (IntervalIndex UTCTime Scheduled))
readSchedule conn scheduleId =
  runRedis conn $ do
    scheduledOps <- get (scheduleIdKey scheduleId)
    pure $ case scheduledOps of
      Left _ -> Nothing
      Right v -> v >>= intervalIndexFromByteString

writeSchedule ::
  Connection ->
  ScheduleId ->
  IntervalIndex UTCTime Scheduled ->
  IO ()
writeSchedule conn scheduleId schedule =
  void . runRedis conn $
    set
      (scheduleIdKey scheduleId)
      (toStrict . encode $ IntervalIndex.allIntervals schedule)

newtype RedisScheduleRepository = RedisScheduleRepository {conn :: Connection}

instance ScheduleRepository RedisScheduleRepository where
  readSchedule (RedisScheduleRepository {conn}) scheduleId =
    liftIO $ SatSim.Cache.readSchedule conn scheduleId
  writeSchedule (RedisScheduleRepository {conn}) scheduleId schedule =
    liftIO $ SatSim.Cache.writeSchedule conn scheduleId schedule
