{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Algo.Greedy where

import Data.Interval (IntervalLit (..))
import Data.IntervalIndex (IntervalIndex, touching)
import Data.List (sortOn)
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime)
import Data.Validation (Validation)
import SatSim.Satellite (Satellite (..), completeCircleTime)
import SatSim.Schedulable (Schedulable (..), ScheduleError, Scheduled, scheduleAt)

scheduleOne :: Satellite -> Schedulable -> IntervalIndex UTCTime Scheduled -> Validation [ScheduleError] (IntervalIndex UTCTime Scheduled)
scheduleOne satellite schedulable@(Schedulable {vector, startCollectAfter, startCollectBefore}) existingSchedule =
  let halfCircleTime = completeCircleTime satellite / 2
      nearbySchedule =
        existingSchedule
          `touching` ( IntervalLit
                         { start = (negate . realToFrac $ halfCircleTime) `addUTCTime` startCollectAfter,
                           end = realToFrac halfCircleTime `addUTCTime` startCollectBefore
                         }
                     )
   in if null nearbySchedule then scheduleAt schedulable vector startCollectAfter existingSchedule else undefined

scheduleOn :: Satellite -> [Schedulable] -> IntervalIndex UTCTime Scheduled -> IntervalIndex UTCTime Scheduled
scheduleOn satellite candidates existingSchedule =
  let sortedCandidates = sortOn arrivalOrder candidates
      scheduleOneCandidate (Schedulable {vector, closeEnough, startCollectAfter, startCollectBefore}) =
        -- find ops from completeCircleTime / 2 before the candidate to completeCircleTime / 2 after the candidate
        -- from the latest one before, see if it's possible to get to the candidate from the end of it, then get to
        -- the next op after the candidate ends
        -- if it is, schedule the candidate at that time
        -- if it's not, keep going
        undefined
   in undefined
