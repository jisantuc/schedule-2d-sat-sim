module SatSim.Algo.GreedySpec (spec) where

import SatSim.Schedulable (Scheduled (..))
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
  it "behaves" $
    True `shouldBe` True
