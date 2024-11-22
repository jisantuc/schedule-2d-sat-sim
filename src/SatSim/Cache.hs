module SatSim.Cache where

import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString (toStrict)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Functor (void)
import Data.IntervalIndex (IntervalIndex)
import qualified Data.IntervalIndex as IntervalIndex
import Data.Time (UTCTime)
import Database.Redis (Connection, get, runRedis, set)
import Debug.Trace (traceShow)
import SatSim.Schedulable (Scheduled)

newtype ScheduleId = ScheduleId String

intervalIndexFromByteString :: BS.ByteString -> Maybe (IntervalIndex UTCTime Scheduled)
intervalIndexFromByteString bs = case eitherDecodeStrict' bs of
  Right v -> Just $ IntervalIndex.fromList v
  Left e -> traceShow e Nothing

scheduleIdKey :: ScheduleId -> BS.ByteString
scheduleIdKey (ScheduleId i) = pack i

readSchedule :: Connection -> ScheduleId -> IO (Maybe (IntervalIndex UTCTime Scheduled))
readSchedule conn scheduleId =
  runRedis conn $ do
    scheduledOps <- get (scheduleIdKey scheduleId)
    pure $ case scheduledOps of
      Left err -> traceShow err Nothing
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
