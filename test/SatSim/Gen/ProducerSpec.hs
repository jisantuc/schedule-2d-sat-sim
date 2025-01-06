{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Gen.ProducerSpec where

import Conduit (runConduit, (.|))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.Time (addUTCTime, getCurrentTime)
import SatSim.Gen.Producer (genSchedulable, stdoutBatchProducer, timeProducer)
import SatSim.Quantities (Seconds (..))
import SatSim.Schedulable (Schedulable (..))
import System.IO.Silently (capture_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "ProducerSpec" $ do
  it "generates the right number of targets" $
    getCurrentTime >>= \currentTime -> do
      targets <- genSchedulable currentTime 100 3
      length targets `shouldBe` 3
  it "generates targets in the right arrival order" $
    let numTargets = 23
     in getCurrentTime >>= \currentTime -> do
          targets <- genSchedulable currentTime 5000 23
          arrivalOrder <$> targets `shouldBe` [1 .. numTargets]
  it "generates targets in the allowed time range" $
    let numTargets = 200
     in getCurrentTime >>= \currentTime -> do
          targets <- genSchedulable currentTime 200 numTargets
          traverse_
            ( \(Schedulable {startCollectAfter}) -> do
                startCollectAfter < addUTCTime 200 currentTime `shouldBe` True
                startCollectAfter > currentTime `shouldBe` True
            )
            targets
  it "produces a stream of outputs" $
    do
      output <-
        capture_
          ( race
              (forever . runConduit $ timeProducer 2 .| stdoutBatchProducer (Seconds 10))
              (threadDelay 5000000)
          )
      -- in 5 seconds, producing every 2 seconds should have happened twice
      (length . lines $ output) `shouldBe` 2
