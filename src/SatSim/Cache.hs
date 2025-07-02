{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Cache
  ( RedisScheduleRepository (..),
    readSchedule,
    writeSchedule,
    LocalScheduleRepository (..),
    scheduleIdKey,
  )
where

import Control.Concurrent (MVar)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString (toStrict)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Functor (void)
import Data.IntervalIndex (IntervalIndex)
import qualified Data.IntervalIndex as IntervalIndex
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Database.Redis (Connection, get, runRedis, set)
import SatSim.Schedulable (Schedule, ScheduleId (..), Scheduled)

intervalIndexFromByteString :: BS.ByteString -> Maybe Schedule
intervalIndexFromByteString bs = case eitherDecodeStrict' bs of
  Right v -> Just $ IntervalIndex.fromList v
  Left _ -> Nothing

scheduleIdKey :: ScheduleId -> BS.ByteString
scheduleIdKey (ScheduleId i) = pack i

readSchedule :: RedisScheduleRepository -> ScheduleId -> IO (Maybe Schedule)
readSchedule (RedisScheduleRepository {conn}) scheduleId =
  runRedis conn $ do
    scheduledOps <- get (scheduleIdKey scheduleId)
    pure $ case scheduledOps of
      Left _ -> Nothing
      Right v -> v >>= intervalIndexFromByteString

writeSchedule ::
  RedisScheduleRepository ->
  ScheduleId ->
  IntervalIndex UTCTime Scheduled ->
  IO ()
writeSchedule (RedisScheduleRepository {conn}) scheduleId schedule =
  void . runRedis conn $
    set
      (scheduleIdKey scheduleId)
      (toStrict . encode $ IntervalIndex.allIntervals schedule)

newtype RedisScheduleRepository = RedisScheduleRepository {conn :: Connection}

newtype LocalScheduleRepository = LocalScheduleRepository {unMVar :: MVar (Map.Map ScheduleId Schedule)}
