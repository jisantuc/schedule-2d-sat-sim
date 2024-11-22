{-# LANGUAGE NamedFieldPuns #-}

module SatSim.CacheSpec where

import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IntervalIndex (IntervalIndex, allIntervals, fromList)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Redis (checkedConnect, defaultConnectInfo)
import SatSim.Cache (ScheduleId (..), readSchedule, writeSchedule)
import SatSim.Quantities (Radians (..))
import SatSim.Schedulable (Schedulable (..), Scheduled (..))
import SatSim.TargetVector (TargetVector, mkTargetVector)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, listOf, sample')

genTargetVector :: Gen TargetVector
genTargetVector = mkTargetVector <$> choose (0.01, 1) <*> choose (0.01, 1)

genSchedulable :: UTCTime -> Gen Schedulable
genSchedulable windowStart =
  Schedulable
    <$> genTargetVector
    <*> (Radians <$> arbitrary)
    <*> pure windowStart
    <*> ((`addUTCTime` windowStart) . fromRational <$> arbitrary)

genScheduled :: UTCTime -> Gen Scheduled
genScheduled windowStart =
  genSchedulable windowStart <&> \schedulable@(Schedulable {vector, startCollectAfter}) ->
    Scheduled schedulable startCollectAfter vector

genScheduleIndex :: UTCTime -> Gen (IntervalIndex UTCTime Scheduled)
genScheduleIndex windowStart = do
  listOf (genScheduled windowStart) <&> fromList

spec :: Spec
spec =
  describe "CacheSpec" $ do
    it "round trips a schedule" $
      do
        conn <- checkedConnect defaultConnectInfo
        currentTime <- getCurrentTime
        schedules <- take 20 <$> sample' (genScheduleIndex currentTime)
        traverse_
          ( \schedule -> do
              writeSchedule conn (ScheduleId "abcde") schedule
              fromCache <- readSchedule conn (ScheduleId "abcde")
              allIntervals <$> fromCache `shouldBe` Just (allIntervals schedule)
          )
          schedules
