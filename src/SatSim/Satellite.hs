module SatSim.Satellite where

import SatSim.Quantities (Radians)

newtype SatelliteName = SatelliteName String deriving (Eq, Show)

data Satellite = SimpleSatellite
  { rotationRatePerSecond :: Radians,
    name :: SatelliteName
  }
  deriving (Eq, Show)
