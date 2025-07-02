{-# LANGUAGE LambdaCase #-}

module SatSim.TestLib (integrationTest) where

import System.Environment (lookupEnv)
import Test.Hspec (pendingWith)

integrationTest :: IO () -> IO ()
integrationTest t =
  lookupEnv "ENV" >>= \case
    Just "local" -> t
    Just "ci" -> t
    Just e -> fail $ "Unexpected env value: " <> e
    Nothing -> pendingWith "skipping integration test"
