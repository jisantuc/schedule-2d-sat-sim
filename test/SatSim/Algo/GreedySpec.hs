module SatSim.Algo.GreedySpec (spec) where

import qualified Data.IntervalIndex as IntervalIndex
import Data.These (These (That))
import Data.Time (UTCTime (UTCTime), addUTCTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import SatSim.Algo.Greedy (scheduleOn, vToT)
import SatSim.Quantities (Radians (..))
import SatSim.Satellite (Satellite (..), SatelliteName (..))
import SatSim.Schedulable (Schedulable (..), scheduleAt)
import SatSim.TargetVector (mkTargetVector)
import Test.Hspec (Spec, describe, it, shouldBe)

--
-- Tests:
--   - scheduling nothing against an empty schedule successfully returns an empty schedule
--   - scheduling one thing against an empty schedule schedules the thing at its start
--   - when trying to schedule something against a schedule with ops in it already, it gets scheduled:
--     - as early as possible before the first op
--     - as early as possible between two existing ops
--     - as early as possible after the last op
--     - or not at all
spec :: Spec
spec = describe "GreedySpec" $ do
  describe
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
       in do
            it "schedules a single candidate against an empty schedule at the candidate's start" $
              scheduleOn
                (SimpleSatellite (Radians 2) (SatelliteName "simple"))
                [candidate]
                IntervalIndex.empty
                `shouldBe` vToT (scheduleAt candidate point startTime IntervalIndex.empty)
            it "gets nothing from scheduling nothing against an empty schedule" $
              scheduleOn (SimpleSatellite (Radians 2) (SatelliteName "simple")) [] IntervalIndex.empty
                `shouldBe` That IntervalIndex.empty
