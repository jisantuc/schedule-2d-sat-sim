{-# LANGUAGE DeriveGeneric #-}

module SatSim.Schedulable where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import SatSim.Quantities (Radians)
import SatSim.TargetVector (TargetVector)

data Schedulable = Schedulable
  { vector :: TargetVector,
    closeEnough :: Radians,
    startCollectAfter :: LocalTime,
    startCollectBefore :: LocalTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Schedulable

instance FromJSON Schedulable
