{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SatSim.Quantities
  ( Radians (..),
    Seconds (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype Radians = Radians Float deriving (Eq, Fractional, Generic, Num, Show)

instance FromJSON Radians

instance ToJSON Radians

newtype Seconds = Seconds Float deriving (Eq, Generic, Num, Show)

instance FromJSON Seconds

instance ToJSON Seconds
