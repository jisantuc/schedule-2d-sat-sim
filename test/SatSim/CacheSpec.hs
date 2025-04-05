{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module SatSim.CacheSpec where

import Data.ByteString.Char8 (pack)
import Data.Functor (void, (<&>))
import Data.IntervalIndex (IntervalIndex, allIntervals, fromList)
import Data.Time (UTCTime, addUTCTime)
import Database.Redis (checkedConnect, defaultConnectInfo, runRedis, set)
import SatSim.Cache (readSchedule, scheduleIdKey, writeSchedule)
import SatSim.Quantities (Radians (..))
import SatSim.Schedulable (Schedulable (..), Scheduled (..))
import SatSim.ScheduleRepository (ScheduleId (..))
import SatSim.TargetVector (TargetVector, mkTargetVector)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, describe, it, pending, shouldBe)
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
        ifEnabled $ do
          conn <- checkedConnect defaultConnectInfo
          writeSchedule conn (ScheduleId "abcde") schedule
          fromCache <- readSchedule conn (ScheduleId "abcde")
          allIntervals <$> fromCache `shouldBe` Just (allIntervals schedule)
    it "finds nothing for a bogus key" $
      ifEnabled $ do
        conn <- checkedConnect defaultConnectInfo
        fromBogus <- readSchedule conn (ScheduleId "bogus")
        fromBogus `shouldBe` Nothing
    it "silently finds nothing in the face of bad data" $
      ifEnabled $ do
        conn <- checkedConnect defaultConnectInfo
        void . runRedis conn $ set (scheduleIdKey (ScheduleId "lol")) (pack "oh no")
        fromCache <- readSchedule conn (ScheduleId "lol")
        fromCache `shouldBe` Nothing

ifEnabled :: IO () -> IO ()
ifEnabled t =
  lookupEnv "ENV" >>= \case
    Just "local" -> t
    Just "ci" -> t
    Just e -> fail $ "Unexpected env value: " <> e
    Nothing -> pending
