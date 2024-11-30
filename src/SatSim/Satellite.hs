{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Satellite
  ( Satellite (..),
    completeCircleTime,
    rotationRate,
  )
where

import SatSim.Quantities (Radians (..), Seconds (..))

newtype SatelliteName = SatelliteName String deriving (Eq, Show)

data Satellite = SimpleSatellite
  { rotationRatePerSecond :: Radians,
    name :: SatelliteName
  }
  deriving (Eq, Show)

completeCircleTime :: Satellite -> Seconds
completeCircleTime (SimpleSatellite {rotationRatePerSecond}) =
  Seconds (2 * pi / unRadians rotationRatePerSecond)

rotationRate :: Satellite -> Radians
rotationRate (SimpleSatellite {rotationRatePerSecond}) =
  rotationRatePerSecond
