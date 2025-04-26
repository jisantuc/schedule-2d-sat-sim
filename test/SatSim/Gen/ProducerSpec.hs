{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Gen.ProducerSpec where

import Data.Foldable (traverse_)
import Data.Time (addUTCTime, getCurrentTime)
import SatSim.Gen.Producer (genSchedulable)
import SatSim.Schedulable (Schedulable (..))
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
