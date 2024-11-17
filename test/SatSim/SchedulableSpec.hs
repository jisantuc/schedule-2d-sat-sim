module SatSim.SchedulableSpec where

import Test.Hspec (Spec, describe, shouldBe, xit)

spec :: Spec
spec = describe "SchedulableSpec" $ do
  -- test that scheduling something into an empty interval index adds an interval
  -- to that interval index starting at the beginning of the valid period and
  -- an end at (start + duration scheduled)
  xit "schedules constraints at times" $
    True `shouldBe` True
