{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Schedulable (Schedulable (..), Scheduled, scheduleAt) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Interval (Interval (..))
import Data.IntervalIndex (IntervalIndex, insert)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import GHC.Generics (Generic)
import SatSim.Quantities (Radians)
import SatSim.TargetVector (TargetVector)

data Schedulable = Schedulable
  { vector :: TargetVector,
    closeEnough :: Radians,
    startCollectAfter :: UTCTime,
    startCollectBefore :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Schedulable

instance FromJSON Schedulable

data Scheduled = Scheduled
  { constraints :: Schedulable,
    start :: UTCTime,
    pointing :: TargetVector
  }
  deriving (Eq, Show, Generic)

instance Interval UTCTime Scheduled where
  intervalStart = start
  intervalEnd scheduled@(Scheduled {start}) = addUTCTime (duration scheduled) start

scheduleAt ::
  Schedulable ->
  TargetVector ->
  UTCTime ->
  IntervalIndex UTCTime Scheduled ->
  IntervalIndex UTCTime Scheduled
scheduleAt constraints pointing start schedule =
  schedule `insert` Scheduled constraints start pointing

duration :: Scheduled -> NominalDiffTime
duration (Scheduled {constraints, pointing}) = undefined
