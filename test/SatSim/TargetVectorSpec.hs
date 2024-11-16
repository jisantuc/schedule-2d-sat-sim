module SatSim.TargetVectorSpec where

import Data.Functor ((<&>))
import GHC.Float (isFloatFinite)
import SatSim.Quantities (Radians (..), Seconds (..))
import SatSim.TargetVector (TargetVector, angleBetween, mkTargetVector, travelTime)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, choose, oneof, suchThat)

genTargetVectorPair :: Gen (TargetVector, TargetVector)
genTargetVectorPair =
  let inputPairGen =
        oneof
          [ (,) <$> choose (0.01, 1) <*> choose (0.01, 1),
            (,) <$> choose (0.01, 1) <*> choose (-0.01, -1),
            (,) <$> choose (-0.01, -1) <*> choose (0.01, 1),
            (,) <$> choose (-0.01, -1) <*> choose (-0.01, -1)
          ]
   in do
        v1 <- uncurry mkTargetVector <$> inputPairGen
        v2 <- uncurry mkTargetVector <$> inputPairGen
        pure (v1, v2)
          `suchThat` ( \(v1', v2') ->
                         let (Radians angle) = angleBetween v1' v2' in isFloatFinite angle == 1
                     )

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
            ( genTargetVectorPair
                <&> ( \(v1, v2) ->
                        angleBetween v1 v2 `shouldBe` angleBetween v2 v1
                    )
            )
          it "correctly calculates travel times for some simple cases" $ do
            travelTime (Radians pi) north south `shouldBe` Seconds 1
            travelTime (Radians pi) east west `shouldBe` Seconds 1
          prop
            "calculates travel times symmetrically"
            ( genTargetVectorPair
                <&> ( \(v1, v2) ->
                        travelTime (Radians pi / 7) v1 v2 `shouldBe` travelTime (Radians pi / 7) v2 v1
                    )
            )
