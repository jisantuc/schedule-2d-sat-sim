module SatSim.SchedulableSpec where

import Data.Interval (Interval (intervalEnd))
import Data.IntervalIndex (at, empty)
import Data.Time (addUTCTime, getCurrentTime)
import SatSim.Quantities (Radians (..))
import SatSim.Schedulable
  ( Schedulable (..),
    Scheduled (..),
    duration,
    scheduleAt,
  )
import SatSim.TargetVector (mkTargetVector)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "SchedulableSpec" $ do
  it "schedules constraints at times" $
    getCurrentTime >>= \currentTime ->
      let east = mkTargetVector 1 0
          north = mkTargetVector 0 1
          schedulable = Schedulable east (Radians (pi / 2)) currentTime (addUTCTime 30 currentTime)
          result = scheduleAt schedulable north currentTime empty
          scheduleAtStart = result `at` currentTime
          scheduled = head scheduleAtStart
          scheduleJustBeforeEnd = result `at` addUTCTime (-0.0001) (intervalEnd scheduled)
          alsoScheduled = head scheduleJustBeforeEnd
          scheduleAtEnd = result `at` intervalEnd scheduled
       in do
            constraints scheduled `shouldBe` schedulable
            constraints alsoScheduled `shouldBe` schedulable
            scheduleAtEnd `shouldSatisfy` null
  it "increases durations for larger pointing misses" $
    let east = mkTargetVector 1 0
        north = mkTargetVector 0 1
        northeast = mkTargetVector 1 1
        schedulable = Schedulable east undefined undefined undefined
        scheduled1 = Scheduled schedulable undefined north
        scheduled2 = Scheduled schedulable undefined northeast
        tolerance = 0.00001
     in do
          duration scheduled1 - (1 + 8) `shouldSatisfy` (<= tolerance)
          duration scheduled2 - (1 + 4) `shouldSatisfy` (<= tolerance)
