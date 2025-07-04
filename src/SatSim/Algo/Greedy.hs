{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Algo.Greedy where

import Data.Foldable (foldMap', foldl')
import Data.Functor.Alt ((<!>))
import Data.Interval (IntervalLit (..))
import Data.IntervalIndex (IntervalIndex, touching)
import Data.List (sortOn)
import Data.These (These (..))
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime)
import Data.Validation (Validation (..), validation)
import SatSim.Satellite (Satellite (..), completeCircleTime, rotationRate)
import SatSim.Schedulable (Schedulable (..), ScheduleError (..), Scheduled (..), duration, minDuration, scheduleAt)
import SatSim.TargetVector (travelTime)

vToT :: Validation e b -> These e b
vToT = validation This That

scheduleOne ::
  Satellite ->
  Schedulable ->
  IntervalIndex UTCTime Scheduled ->
  These [ScheduleError] (IntervalIndex UTCTime Scheduled)
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
        satelliteTravelTime = travelTime (rotationRate satellite)
        reconcile prior result = case result of
          schedule@(Success _) -> vToT schedule
          errs@(Failure _) -> That prior <> vToT errs
     in reconcile existingSchedule $ case nearbySchedule of
          -- if no nearby schedule, schedule the new thing
          [] ->
            scheduleAt
              schedulable
              vector
              startCollectAfter
              existingSchedule
          -- if exactly one nearby op:
          -- \* if the nearby op starts after startCollectAfter, check if
          --   start of nearby op - travelTime candidatePointing opPointing is after candidate scheduled at start
          -- \* if it is, schedule at the start
          -- \* if not, try to schedule at start + duration + travelTime between pointings
          [scheduled@(Scheduled _ start pointing)] ->
            let travelTimeBetweenPoints = realToFrac . satelliteTravelTime vector $ pointing
                earliestStartAfterFirstOp = addUTCTime travelTimeBetweenPoints (addUTCTime (duration scheduled) start)
             in scheduleAt
                  schedulable
                  vector
                  -- earliest of start time or required departure time
                  ( min
                      -- departure time
                      ( addUTCTime
                          (negate travelTimeBetweenPoints)
                          $ addUTCTime (negate minDuration) start
                      )
                      -- window open
                      startCollectAfter
                  )
                  existingSchedule
                  <!>
                  -- alternatively, schedule at the max of op end + travel time to candidate or earliest allowed
                  -- start for second op
                  scheduleAt
                    schedulable
                    vector
                    (max earliestStartAfterFirstOp startCollectAfter)
                    existingSchedule
          -- otherwise:
          -- otherwise, go through pairs of ops trying to schedule between them,
          -- making sure:
          --   * end of first op + duration candidate + travelTime candidate secondOp all fits
          schedule@(Scheduled _ start pointing : _) ->
            let withNext = zip schedule (drop 1 schedule)
             in -- try to schedule before the first op, if possible
                scheduleAt
                  schedulable
                  vector
                  -- earlier of start time or required departure time
                  ( min
                      -- departure time
                      ( addUTCTime
                          (negate . realToFrac . satelliteTravelTime vector $ pointing)
                          $ addUTCTime (negate minDuration) start
                      )
                      -- window open
                      startCollectAfter
                  )
                  existingSchedule
                  <!> foldMap'
                    ( \(scheduled1@(Scheduled _ start1 pointing1), Scheduled _ start2 pointing2) ->
                        let earliestStartOfOp =
                              addUTCTime
                                ( duration scheduled1
                                    + realToFrac (satelliteTravelTime pointing1 vector)
                                )
                                start1
                            earliestEndOfOp = addUTCTime minDuration earliestStartOfOp
                            earliestArrivalAtNextOp =
                              addUTCTime
                                (realToFrac $ satelliteTravelTime vector pointing2)
                                earliestEndOfOp
                         in if earliestArrivalAtNextOp <= start2
                              then scheduleAt schedulable vector earliestStartOfOp existingSchedule
                              else Failure [CrowdedOut]
                    )
                    withNext

scheduleOn ::
  Satellite ->
  [Schedulable] ->
  IntervalIndex UTCTime Scheduled ->
  These [ScheduleError] (IntervalIndex UTCTime Scheduled)
scheduleOn _ [] existingSchedule = That existingSchedule
scheduleOn satellite candidates existingSchedule =
  let sortedCandidates = sortOn arrivalOrder candidates
   in foldl'
        ( \acc candidate ->
            case acc of
              errs@(This _) -> errs <> scheduleOne satellite candidate existingSchedule
              (That schedule) -> scheduleOne satellite candidate schedule
              These errs schedule -> This errs <> scheduleOne satellite candidate schedule
        )
        (That existingSchedule)
        sortedCandidates
