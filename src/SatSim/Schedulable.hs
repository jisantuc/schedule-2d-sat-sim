{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Schedulable
  ( Schedulable (..),
    Scheduled (..),
    duration,
    scheduleAt,
    strictScheduleAt,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Interval (Interval (..))
import Data.IntervalIndex (IntervalIndex, insert)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Validation (Validation, validate)
import GHC.Generics (Generic)
import SatSim.Quantities (Radians (unRadians))
import SatSim.TargetVector (TargetVector, angleBetween)

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

data ScheduleError
  = PointingOutOfBounds
  | StartTimeOutOfBounds

scheduleAt ::
  Schedulable ->
  TargetVector ->
  UTCTime ->
  IntervalIndex UTCTime Scheduled ->
  IntervalIndex UTCTime Scheduled
scheduleAt constraints pointing start schedule =
  schedule `insert` Scheduled constraints start pointing

strictScheduleAt ::
  Schedulable ->
  TargetVector ->
  UTCTime ->
  IntervalIndex UTCTime Scheduled ->
  Validation [ScheduleError] (IntervalIndex UTCTime Scheduled)
strictScheduleAt constraints pointing start schedule = do
  validate
    [PointingOutOfBounds]
    ( \constr ->
        if angleBetween (vector constr) pointing <= closeEnough constr
          then Just ()
          else Nothing
    )
    constraints
  validate
    [StartTimeOutOfBounds]
    ( \constr ->
        if start < startCollectAfter constr || start >= startCollectBefore constr
          then Nothing
          else Just ()
    )
    constraints
  pure $ scheduleAt constraints pointing start schedule

duration :: Scheduled -> NominalDiffTime
duration (Scheduled {constraints, pointing}) =
  let angularDifference = angleBetween (vector constraints) pointing
      increment = realToFrac $ 8 * unRadians angularDifference / pi
   in 1 + increment
