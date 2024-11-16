{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SatSim.Quantities
  ( Radians (..),
    Seconds (..),
  )
where

newtype Radians = Radians Float deriving (Eq, Fractional, Num, Show)

newtype Seconds = Seconds Float deriving (Eq, Num, Show)
