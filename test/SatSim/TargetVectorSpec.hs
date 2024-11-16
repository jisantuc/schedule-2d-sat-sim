module SatSim.TargetVectorSpec where

import SatSim.Quantities (Radians (..), Seconds (..))
import SatSim.TargetVector (angleBetween, mkTargetVector, travelTime)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonZero (NonZero))

spec :: Spec
spec =
  describe "TargetVectorSpec" $
    let east = mkTargetVector 1 0
        west = mkTargetVector (-1) 0
        north = mkTargetVector 0 1
        south = mkTargetVector 0 (-1)
     in do
          it "calculates angles between target vectors in radians" $ do
            angleBetween east north `shouldBe` Radians (pi / 2)
            angleBetween east west `shouldBe` Radians pi
            angleBetween east south `shouldBe` Radians (pi / 2)
            angleBetween north west `shouldBe` Radians (pi / 2)
            angleBetween north south `shouldBe` Radians pi
            angleBetween west south `shouldBe` Radians (pi / 2)
          prop
            "calculates angles between target vectors symmetricaly"
            ( \(NonZero x1) (NonZero y1) (NonZero x2) (NonZero y2) ->
                let v1 = mkTargetVector x1 y1
                    v2 = mkTargetVector x2 y2
                 in angleBetween v1 v2 `shouldBe` angleBetween v2 v1
            )
          it "correctly calculates travel times for some simple cases" $ do
            travelTime (Radians pi) north south `shouldBe` Seconds 1
            travelTime (Radians pi) east west `shouldBe` Seconds 1
          prop
            "calculates travel times symmetrically"
            ( \(NonZero x1) (NonZero y1) (NonZero x2) (NonZero y2) ->
                let v1 = mkTargetVector x1 y1
                    v2 = mkTargetVector x2 y2
                 in travelTime (Radians pi / 7) v1 v2 `shouldBe` travelTime (Radians pi / 7) v2 v1
            )
