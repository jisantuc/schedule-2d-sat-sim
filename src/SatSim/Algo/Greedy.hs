{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Algo.Greedy where

import Control.Applicative ((<|>))
import Data.Functor.Alt ((<!>))
import Data.Interval (IntervalLit (..))
import Data.IntervalIndex (IntervalIndex, touching)
import Data.List (sortOn)
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime)
import Data.Validation (Validation (..))
import SatSim.Satellite (Satellite (..), completeCircleTime, rotationRate)
import SatSim.Schedulable (Schedulable (..), ScheduleError (PointingOutOfBounds), Scheduled (..), duration, minDuration, scheduleAt)
import SatSim.TargetVector (travelTime)

scheduleOne :: Satellite -> Schedulable -> IntervalIndex UTCTime Scheduled -> Validation [ScheduleError] (IntervalIndex UTCTime Scheduled)
scheduleOne
  satellite
  schedulable@( Schedulable
                  { vector,
                    startCollectAfter,
                    startCollectBefore
                  }
                )
  existingSchedule =
    let halfCircleTime = completeCircleTime satellite / 2
        nearbySchedule =
          existingSchedule
            `touching` IntervalLit
              ((negate . realToFrac $ halfCircleTime) `addUTCTime` startCollectAfter)
              (realToFrac halfCircleTime `addUTCTime` startCollectBefore)
     in case nearbySchedule of
          -- if no nearby schedule, schedule the new thing
          [] ->
            scheduleAt
              schedulable
              vector
              startCollectAfter
              existingSchedule
          -- if exactly one nearby op:
          -- \* if the nearby op starts after startCollectAfter, check if
          --   start of nearby op - travelTime candidatePointing opPointing is after candidate scheduled at start -- if it is, schedule at the start!
          -- \* if not, try to schedule at start + duration + travelTime between pointings
          [scheduled@(Scheduled _ start pointing)] ->
            scheduleAt
              schedulable
              vector
              -- earlier of start time or required departure time
              ( min
                  -- departure time
                  ( addUTCTime
                      (negate . realToFrac . travelTime (rotationRate satellite) vector $ pointing)
                      $ addUTCTime (negate minDuration) start
                  )
                  -- window open
                  startCollectAfter
              )
              existingSchedule
              <!>
              -- alternatively, schedule at the min of op end + travel time to candidate (always want earliest, so no
              -- min/max required here)
              scheduleAt
                schedulable
                vector
                (addUTCTime ((realToFrac . travelTime (rotationRate satellite) vector) pointing) (addUTCTime (duration scheduled) start))
                existingSchedule
          -- otherwise:
          -- try to schedule before the first op, if possible
          -- otherwise, go through pairs of ops trying to schedule between them,
          -- making sure:
          --   * end of first op + duration candidate + travelTime candidate secondOp all fits
          schedule -> Failure [PointingOutOfBounds]

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
