module SatSim.Algo.GreedySpec (spec) where

import qualified Data.IntervalIndex as IntervalIndex
import Data.These (These (..))
import Data.Time (UTCTime (..), addUTCTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Validation (bindValidation)
import SatSim.Algo.Greedy (scheduleOn, vToT)
import SatSim.Quantities (Radians (..))
import SatSim.Satellite (Satellite (..), SatelliteName (..), rotationRate)
import SatSim.Schedulable (Schedulable (..), ScheduleError (..), minDuration, scheduleAt)
import SatSim.TargetVector (mkTargetVector, travelTime)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "GreedySpec"
    $ describe
      "schedules at the right times"
    $ let startTime = UTCTime (fromOrdinalDate 2024 304) 2934
          endTime = addUTCTime 100000 startTime
          point = mkTargetVector 1 3
          candidate =
            Schedulable
              { vector = point,
                startCollectBefore = endTime,
                startCollectAfter = startTime,
                closeEnough = Radians 1,
                arrivalOrder = 1
              }
          fastSatellite = SimpleSatellite (Radians 2) (SatelliteName "fast")
          slowSatellite = SimpleSatellite (Radians 0.001) (SatelliteName "fast")
       in do
            it "gets nothing from scheduling nothing against an empty schedule" $
              scheduleOn (SimpleSatellite (Radians 2) (SatelliteName "simple")) [] IntervalIndex.empty
                `shouldBe` That IntervalIndex.empty
            it "schedules a single candidate against an empty schedule at the candidate's start" $
              scheduleOn
                fastSatellite
                [candidate]
                IntervalIndex.empty
                `shouldBe` vToT (scheduleAt candidate point startTime IntervalIndex.empty)
            it "schedules two candidates that don't conflict at each of their starts" $
              let nonConflictingCandidate =
                    candidate
                      { startCollectAfter = addUTCTime 100000 (startCollectBefore candidate),
                        startCollectBefore = addUTCTime 200000 (startCollectBefore candidate)
                      }
               in scheduleOn
                    fastSatellite
                    [candidate, nonConflictingCandidate]
                    IntervalIndex.empty
                    -- It should be like scheduling them both against empty schedules and combining the result
                    `shouldBe` vToT
                      ( scheduleAt candidate point startTime IntervalIndex.empty
                          `bindValidation` scheduleAt
                            nonConflictingCandidate
                            point
                            (startCollectAfter nonConflictingCandidate)
                      )
            it "schedules two identical candidates back-to-back" $
              scheduleOn fastSatellite [candidate, candidate] IntervalIndex.empty
                `shouldBe` vToT
                  ( scheduleAt
                      candidate
                      point
                      startTime
                      IntervalIndex.empty
                      `bindValidation` scheduleAt
                        candidate
                        point
                        (addUTCTime minDuration $ startCollectAfter candidate)
                  )
            it "schedules two nearby candidates with just the travel time between them" $
              let altPoint = mkTargetVector 1 4
                  nearbyCandidate = candidate {vector = altPoint}
               in scheduleOn fastSatellite [candidate, nearbyCandidate] IntervalIndex.empty
                    `shouldBe` vToT
                      ( scheduleAt candidate point startTime IntervalIndex.empty
                          `bindValidation` scheduleAt
                            nearbyCandidate
                            altPoint
                            ( addUTCTime
                                (minDuration + realToFrac (travelTime (Radians 2) point altPoint))
                                (startCollectAfter candidate)
                            )
                      )
            it "schedules one candidate between two other candidates when it can" $
              let sharedTarget = mkTargetVector 2 (-11)
                  midTarget = mkTargetVector 3 (-8)
                  candidateStart = candidate {vector = sharedTarget}
                  candidateMid =
                    candidateStart
                      { vector =
                          midTarget,
                        startCollectBefore = addUTCTime 30 (startCollectAfter candidateStart)
                      }
                  candidateEnd =
                    candidate
                      { vector =
                          sharedTarget,
                        startCollectAfter = addUTCTime 20 (startCollectAfter candidateStart)
                      }
               in scheduleOn
                    fastSatellite
                    [candidateStart, candidateEnd, candidateMid]
                    IntervalIndex.empty
                    `shouldBe` scheduleOn fastSatellite [candidateStart, candidateEnd] IntervalIndex.empty
                      <> vToT
                        ( scheduleAt
                            candidateMid
                            midTarget
                            ( addUTCTime
                                ( minDuration
                                    + realToFrac (travelTime (rotationRate fastSatellite) sharedTarget midTarget)
                                )
                                (startCollectAfter candidateStart)
                            )
                            IntervalIndex.empty
                        )
            it "keeps scheduling past the first error" $
              let shortDurationCandidate =
                    candidate
                      { startCollectBefore = addUTCTime 1 $ startCollectAfter candidate
                      }
               in scheduleOn
                    fastSatellite
                    [shortDurationCandidate, shortDurationCandidate, candidate]
                    IntervalIndex.empty
                    `shouldBe` ( scheduleOn fastSatellite [shortDurationCandidate, candidate] IntervalIndex.empty
                                   <> This [StartTimeOutOfBounds]
                               )
            it "doesn't schedule a candidate between two other candidates when it can't get away from there in time" $
              let sharedTarget = mkTargetVector 0 1
                  midTarget = mkTargetVector 1 0
                  candidateStart =
                    candidate {vector = sharedTarget}
                  -- the mid candidate can start at the same time as the start candidate but must start before the end
                  -- candidate
                  candidateMid =
                    candidateStart
                      { vector = midTarget,
                        startCollectAfter =
                          addUTCTime 1 $ startCollectAfter candidateStart,
                        startCollectBefore =
                          addUTCTime 10 $ startCollectAfter candidateStart
                      }
                  candidateEnd =
                    candidateStart
                      { startCollectAfter = addUTCTime 10 $ startCollectAfter candidateStart,
                        startCollectBefore = addUTCTime 1000 $ startCollectAfter candidateStart,
                        vector = sharedTarget
                      }
                  -- the satellite should be fast enough to get to candidateMid
                  -- the targets are pi / 2 radians apart, so if it takes 8 seconds to get there, that should be
                  -- enough time to make sure it can't rotate back to end candidate
                  tooSlowSatellite = slowSatellite {rotationRatePerSecond = Radians (pi / 16)}
               in do
                    scheduleOn tooSlowSatellite [candidateStart] IntervalIndex.empty
                      `shouldBe` vToT
                        ( scheduleAt
                            candidateStart
                            (vector candidateStart)
                            (startCollectAfter candidateStart)
                            IntervalIndex.empty
                        )
                    scheduleOn tooSlowSatellite [candidateStart, candidateEnd] IntervalIndex.empty
                      `shouldBe` vToT
                        ( scheduleAt
                            candidateStart
                            (vector candidateStart)
                            (startCollectAfter candidateStart)
                            IntervalIndex.empty
                            `bindValidation` scheduleAt
                              candidateEnd
                              (vector candidateEnd)
                              (startCollectAfter candidateEnd)
                        )
                    scheduleOn tooSlowSatellite [candidateStart, candidateEnd, candidateMid] IntervalIndex.empty
                      `shouldBe` scheduleOn tooSlowSatellite [candidateStart, candidateEnd] IntervalIndex.empty
                        <> This [CrowdedOut]
