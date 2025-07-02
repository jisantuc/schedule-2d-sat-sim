{-# LANGUAGE NamedFieldPuns #-}

module SatSim.CacheSpec where

import Data.ByteString.Char8 (pack)
import Data.Functor (void, (<&>))
import Data.IntervalIndex (IntervalIndex, allIntervals, fromList)
import Data.Time (UTCTime, addUTCTime)
import Database.Redis (checkedConnect, defaultConnectInfo, runRedis, set)
import SatSim.Cache (RedisScheduleRepository (..), readSchedule, scheduleIdKey, writeSchedule)
import SatSim.Quantities (Radians (..))
import SatSim.Schedulable (Schedulable (..), ScheduleId (..), Scheduled (..))
import SatSim.TargetVector (TargetVector, mkTargetVector)
import SatSim.TestLib (integrationTest)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, listOf)
import Test.QuickCheck.Instances.Time ()

genTargetVector :: Gen TargetVector
genTargetVector = mkTargetVector <$> choose (0.01, 1) <*> choose (0.01, 1)

genSchedulable :: Gen Schedulable
genSchedulable = do
  target <- genTargetVector
  tolerance <- Radians <$> arbitrary
  open <- arbitrary
  close <- (`addUTCTime` open) . fromRational <$> arbitrary
  pure $ Schedulable target tolerance open close 1

genScheduled :: Gen Scheduled
genScheduled =
  genSchedulable <&> \schedulable@(Schedulable {vector, startCollectAfter}) ->
    Scheduled schedulable startCollectAfter vector

genScheduleIndex :: Gen (IntervalIndex UTCTime Scheduled)
genScheduleIndex = do
  listOf genScheduled <&> fromList

spec :: Spec
spec =
  describe "CacheSpec" $ do
    prop "round trips a schedule" $
      genScheduleIndex <&> \schedule ->
        integrationTest $ do
          conn <- checkedConnect defaultConnectInfo
          writeSchedule (RedisScheduleRepository {conn}) (ScheduleId "abcde") schedule
          fromCache <- readSchedule (RedisScheduleRepository {conn}) (ScheduleId "abcde")
          allIntervals <$> fromCache `shouldBe` Just (allIntervals schedule)
    it "finds nothing for a bogus key" $
      integrationTest $ do
        conn <- checkedConnect defaultConnectInfo
        fromBogus <- readSchedule (RedisScheduleRepository {conn}) (ScheduleId "bogus")
        fromBogus `shouldBe` Nothing
    it "silently finds nothing in the face of bad data" $
      integrationTest $ do
        conn <- checkedConnect defaultConnectInfo
        void . runRedis conn $ set (scheduleIdKey (ScheduleId "lol")) (pack "oh no")
        fromCache <- readSchedule (RedisScheduleRepository {conn}) (ScheduleId "lol")
        fromCache `shouldBe` Nothing
