module SatSim.ScheduleRepository where

import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.IntervalIndex (IntervalIndex)
import Data.Time (UTCTime)
import SatSim.Cache (RedisScheduleRepository (..))
import qualified SatSim.Cache as Cache
import SatSim.Schedulable (ScheduleId, Scheduled)

type Schedule = IntervalIndex UTCTime Scheduled

class ScheduleRepository m where
  readSchedule :: ScheduleId -> m (Maybe Schedule)
  writeSchedule :: ScheduleId -> Schedule -> m ()

instance ScheduleRepository (ReaderT RedisScheduleRepository IO) where
  readSchedule scheduleId = do
    (RedisScheduleRepository conn) <- ask
    readSchedule conn scheduleId
