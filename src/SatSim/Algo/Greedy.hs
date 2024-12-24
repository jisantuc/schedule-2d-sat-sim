{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Algo.Greedy where

import Data.Foldable (foldl')
import Data.Functor.Alt ((<!>))
import Data.Interval (IntervalLit (..))
import Data.IntervalIndex (IntervalIndex, touching)
import qualified Data.IntervalIndex as IntervalIndex
import Data.List (sortOn)
import Data.These (These (..))
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime)
import Data.Validation (Validation (..), validation)
import SatSim.Satellite (Satellite (..), completeCircleTime, rotationRate)
import SatSim.Schedulable (Schedulable (..), ScheduleError (PointingOutOfBounds), Scheduled (..), duration, minDuration, scheduleAt)
import SatSim.TargetVector (travelTime)

vToT :: Validation e b -> These e b
vToT = validation This That

scheduleOne :: Satellite -> Schedulable -> IntervalIndex UTCTime Scheduled -> These [ScheduleError] (IntervalIndex UTCTime Scheduled)
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
     in vToT $ case nearbySchedule of
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

scheduleOn :: Satellite -> [Schedulable] -> IntervalIndex UTCTime Scheduled -> These [ScheduleError] (IntervalIndex UTCTime Scheduled)
scheduleOn _ [] existingSchedule = That existingSchedule
scheduleOn satellite candidates existingSchedule =
  let sortedCandidates = sortOn arrivalOrder candidates
   in foldl'
        ( \acc candidate ->
            acc <> scheduleOne satellite candidate existingSchedule
        )
        (That IntervalIndex.empty)
        sortedCandidates
