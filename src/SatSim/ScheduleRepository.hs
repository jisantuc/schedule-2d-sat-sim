module SatSim.ScheduleRepository where

import Control.Monad.IO.Class (MonadIO)
import Data.IntervalIndex (IntervalIndex)
import Data.Time (UTCTime)
import SatSim.Schedulable (Scheduled)

type Schedule = IntervalIndex UTCTime Scheduled

newtype ScheduleId = ScheduleId String

class ScheduleRepository a where
  readSchedule :: (MonadIO m) => a -> ScheduleId -> m (Maybe Schedule)
  writeSchedule :: (MonadIO m) => a -> ScheduleId -> Schedule -> m ()
