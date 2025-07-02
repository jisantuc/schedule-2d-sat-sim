{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module SatSim.Schedulable
  ( Schedulable (..),
    Scheduled (..),
    ScheduleError (..),
    ScheduleId (..),
    Schedule,
    duration,
    minDuration,
    unsafeScheduleAt,
    scheduleAt,
  )
where

import Data.Aeson (FromJSON, ToJSON (..))
import Data.Interval (Interval (..))
import Data.IntervalIndex (IntervalIndex, insert)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Validation (Validation, validate)
import GHC.Generics (Generic)
import SatSim.Quantities (Radians (unRadians))
import SatSim.TargetVector (TargetVector, angleBetween)

newtype ScheduleId = ScheduleId String deriving (Eq, Ord)

type Schedule = IntervalIndex UTCTime Scheduled

data Schedulable = Schedulable
  { vector :: TargetVector,
    closeEnough :: Radians,
    startCollectAfter :: UTCTime,
    startCollectBefore :: UTCTime,
    arrivalOrder :: Int
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

instance ToJSON Scheduled

instance FromJSON Scheduled

instance Interval UTCTime Scheduled where
  intervalStart = start
  intervalEnd scheduled@(Scheduled {start}) = addUTCTime (duration scheduled) start

data ScheduleError
  = PointingOutOfBounds
  | StartTimeOutOfBounds
  | CrowdedOut
  deriving (Eq, Show)

unsafeScheduleAt ::
  Schedulable ->
  TargetVector ->
  UTCTime ->
  IntervalIndex UTCTime Scheduled ->
  IntervalIndex UTCTime Scheduled
unsafeScheduleAt constraints pointing start schedule =
  schedule `insert` Scheduled constraints start pointing

scheduleAt ::
  Schedulable ->
  TargetVector ->
  UTCTime ->
  IntervalIndex UTCTime Scheduled ->
  Validation [ScheduleError] (IntervalIndex UTCTime Scheduled)
scheduleAt constraints pointing start schedule = do
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
  pure $ unsafeScheduleAt constraints pointing start schedule

duration :: Scheduled -> NominalDiffTime
duration (Scheduled {constraints, pointing}) =
  let angularDifference = angleBetween (vector constraints) pointing
      increment = realToFrac $ 8 * unRadians angularDifference / pi
   in minDuration + increment

minDuration :: NominalDiffTime
minDuration = 1
