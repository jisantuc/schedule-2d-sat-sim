{-# LANGUAGE FlexibleInstances #-}

module SatSim.Core.ConsumerSpec where

import Control.Concurrent (modifyMVar_, newMVar, takeMVar, threadDelay, putMVar)
import Control.Concurrent.Async (race)
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, withReaderT)
import Data.Foldable (foldl')
import Data.These (These (..))
import Data.These.Combinators (justThat)
import Data.Time (getCurrentTime)
import SatSim.Algo.Greedy (scheduleOn)
import SatSim.Cache (LocalScheduleRepository (..))
import SatSim.Core.BatchStreamSource (BatchStreamSource, StaticBatchStream (..), batches)
import SatSim.Core.Consumer (Heartbeat (..), beatForever, consumeBatches)
import SatSim.Core.ScheduleRepository (ScheduleRepository, readSchedule, writeSchedule)
import SatSim.Gen.Producer (genSchedulable)
import SatSim.Satellite (Satellite (SimpleSatellite), SatelliteName (SatelliteName))
import SatSim.Schedulable (ScheduleId (ScheduleId))
import Streamly.Data.Fold (drain)
import Streamly.Data.Stream (fold)
import qualified Streamly.Data.Stream as Stream
import System.IO.Silently (capture_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "ConsumerSpec" $ do
  describe "Consumer" $ do
    it "heartbeats" $
      getCurrentTime >>= \currentTime -> do
        bs <- replicateM 3 $ genSchedulable currentTime 100 3
        mvar <- newMVar mempty
        counterMvar <- newMVar (0 :: Int)
        let heartbeatMessage = "hi"
        let hb = modifyMVar_ counterMvar (\x -> pure (x + 1)) *> print heartbeatMessage
        _ <- capture_ $ race
            ( runReaderT
                (consumeBatches (SimpleSatellite 3 (SatelliteName "test")) (Heartbeat hb 1))
                (TestConsumerConfig (StaticBatchStream bs) (LocalScheduleRepository mvar))
            )
            (threadDelay 3000000)
        hbCalls <- takeMVar counterMvar
        -- for thread timing reasons, sometimes it beats three times, sometimes 4
        hbCalls >= 3 `shouldBe` True
    it "evaluates the schedule" $
      getCurrentTime >>= \currentTime -> do
        bs <- replicateM 3 $ genSchedulable currentTime 100 3
        mvar <- newMVar mempty
        let testConsumerConfig = TestConsumerConfig (StaticBatchStream bs) (LocalScheduleRepository mvar)
        let satellite = SimpleSatellite 3 (SatelliteName "test")
        let hb = pure ()
        _ <-
          race
            ( runReaderT
                (consumeBatches satellite (Heartbeat hb 20))
                testConsumerConfig
            )
            (threadDelay 1000000)
        result <- runReaderT (readSchedule (ScheduleId "schedule-key")) testConsumerConfig
        result
          `shouldBe` justThat
            ( foldl'
                ( \acc batch -> case acc of
                    This _ -> scheduleOn satellite batch mempty
                    That schedule -> scheduleOn satellite batch schedule
                    These _ schedule -> scheduleOn satellite batch schedule
                )
                (That mempty)
                bs
            )
  describe "Heartbeat" $
    it "beats on time" $
      do
        output <-
          capture_
            ( race
                (fold drain $ beatForever (Heartbeat (putStrLn "a") 2))
                (threadDelay 3000000)
            )
        (length . lines $ output) `shouldBe` 2
        lines output `shouldBe` ["a", "a"]

data TestConsumerConfig = TestConsumerConfig
  { staticSource :: StaticBatchStream,
    localScheduleRepository :: LocalScheduleRepository
  }

instance BatchStreamSource (ReaderT TestConsumerConfig IO) where
  batches = Stream.morphInner (withReaderT staticSource) batches

instance ScheduleRepository (ReaderT TestConsumerConfig IO) where
  writeSchedule scheduleId schedule = withReaderT localScheduleRepository $ writeSchedule scheduleId schedule
  readSchedule = withReaderT localScheduleRepository . readSchedule
