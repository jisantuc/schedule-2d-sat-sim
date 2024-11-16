module SatSim.TargetVector
  ( TargetVector,
    mkTargetVector,
    unTargetVector,
    angleBetween,
    travelTime,
  )
where

import SatSim.Quantities (Radians (..), Seconds (..))

newtype TargetVector = TargetVector (Float, Float) deriving (Eq, Show)

magnitude :: TargetVector -> Float
magnitude v =
  let (x, y) = unTargetVector v
   in sqrt (x ** 2 + y ** 2)

unTargetVector :: TargetVector -> (Float, Float)
unTargetVector (TargetVector (x, y)) = (x, y)

dotProduct :: TargetVector -> TargetVector -> Float
dotProduct v1 v2 =
  let (x1, y1) = unTargetVector v1
      (x2, y2) = unTargetVector v2
   in x1 * x2 + y1 * y2

angleBetween :: TargetVector -> TargetVector -> Radians
angleBetween v1 v2 =
  Radians . acos $
    (v1 `dotProduct` v2) / (magnitude v1 * magnitude v2)

travelTime :: Radians -> TargetVector -> TargetVector -> Seconds
travelTime (Radians rotationRateScalar) v1 v2 =
  let (Radians rotationDistance) = angleBetween v1 v2
   in Seconds $ rotationDistance / rotationRateScalar

mkTargetVector :: Float -> Float -> TargetVector
mkTargetVector x y =
  let vectorNorm = magnitude (TargetVector (x, y))
   in TargetVector (x / vectorNorm, y / vectorNorm)
