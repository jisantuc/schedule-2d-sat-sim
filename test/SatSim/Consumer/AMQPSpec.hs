module SatSim.Consumer.AMQPSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import SatSim.Consumer.AMQP (Heartbeat (Heartbeat), beatForever)
import System.IO.Silently (capture_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "AMQPSpec" $ do
  describe "Heartbeat" $ do
    it "beats on time" $
      do
        output <-
          capture_
            ( race
                (beatForever (Heartbeat (putStrLn "a") 2))
                (threadDelay 3000000)
            )
        (length . lines $ output) `shouldBe` 2
        lines output `shouldBe` ["a", "a"]
