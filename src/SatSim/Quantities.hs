{-# LANGUAGE DerivingVia #-}

module SatSim.Quantities
  ( Radians (..),
    Seconds (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import System.Random (Random)

newtype Radians = Radians {unRadians :: Float}
  deriving
    ( Eq,
      Fractional,
      Num,
      Ord,
      Real,
      Show,
      ToJSON,
      FromJSON
    )
    via Float

newtype Seconds = Seconds {unSeconds :: Float}
  deriving
    ( Eq,
      Fractional,
      Num,
      Ord,
      Random,
      Real,
      RealFrac,
      Show,
      ToJSON,
      FromJSON
    )
    via Float
