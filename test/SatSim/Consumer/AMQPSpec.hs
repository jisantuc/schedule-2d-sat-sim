module SatSim.Consumer.AMQPSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import SatSim.Consumer.AMQP (Heartbeat (Heartbeat), beatForever)
import Streamly.Data.Fold (drain)
import Streamly.Data.Stream (fold)
import System.IO.Silently (capture_)
import Test.Hspec (Spec, describe, it, pending, shouldBe)

-- TODO: #16: https://github.com/jisantuc/schedule-2d-sat-sim/issues/16
spec :: Spec
spec = describe "AMQPSpec" $ do
  describe "Consumer" $ do
    it "heartbeats" $ do
      pending
    it "evaluates the schedule" $ do
      pending
  describe "Heartbeat" $ do
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
