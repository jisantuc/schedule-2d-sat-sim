{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Conduit (runConduit, (.|))
import Kafka.Producer
  ( Timeout (..),
    brokersList,
    sendTimeout,
  )
import Options.Applicative
  ( Parser,
    auto,
    command,
    execParser,
    fullDesc,
    helper,
    hsubparser,
    idm,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    (<**>),
  )
import SatSim.Gen.Producer (kafkaBatchProducer, timeProducer)
import SatSim.Quantities (Seconds (..))

data Command
  = ProduceEvery Int Seconds
  | RunScheduler

produceEvery :: Parser Command
produceEvery =
  ProduceEvery
    <$> option auto (long "time-between-batches" <> short 't' <> metavar "BATCH_INTERVAL")
    <*> (Seconds <$> option auto (long "batch-window-size" <> short 'w' <> metavar "BATCH_INTERVAL"))

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "producer"
        ( info
            produceEvery
            ( fullDesc
                <> progDesc "Produce a batch of schedulable tasks every BATCH_INTERVAL seconds"
            )
        )
    )
    <**> helper

runProducer :: Int -> Seconds -> IO ()
runProducer timeBetweenBatches batchWindowSize =
  forever . runConduit $ (timeProducer timeBetweenBatches .| kafkaBatchProducer kafkaSettings batchWindowSize)
  where
    kafkaSettings = brokersList ["localhost:9092"] <> sendTimeout (Timeout 5)

main :: IO ()
main = do
  cmd <- execParser (info commandParser idm)
  case cmd of
    ProduceEvery n s -> runProducer n s
    RunScheduler -> print ("someday" :: String)
