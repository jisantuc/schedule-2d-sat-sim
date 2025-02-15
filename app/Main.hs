{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Conduit (runConduit, (.|))
import Data.List (singleton)
import Kafka.Producer
  ( ProducerProperties (..),
    Timeout (..),
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
    str,
    value,
    (<**>),
  )
import SatSim.Gen.Producer (kafkaBatchProducer, timeProducer)
import SatSim.Quantities (Seconds (..))

data Command
  = ProduceEvery ProducerProperties Int Seconds
  | RunScheduler

producerPropertiesParser :: Parser ProducerProperties
producerPropertiesParser =
  let brokerAddress = option str (long "broker-address" <> metavar "BROKER_PORT" <> value "localhost:9092")
      brokers = brokersList . singleton <$> brokerAddress
      timeout = sendTimeout . Timeout <$> option auto (long "send-timeout-ms" <> metavar "SEND_TIMEOUT" <> value 5)
   in (<>) <$> brokers <*> timeout

produceEvery :: Parser Command
produceEvery =
  ProduceEvery
    <$> producerPropertiesParser
    <*> option auto (long "time-between-batches" <> short 't' <> metavar "BATCH_INTERVAL")
    <*> (Seconds <$> option auto (long "batch-window-size" <> short 'w' <> metavar "BATCH_SIZE"))

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

runProducer :: ProducerProperties -> Int -> Seconds -> IO ()
runProducer kafkaSettings timeBetweenBatches batchWindowSize =
  forever . runConduit $ (timeProducer timeBetweenBatches .| kafkaBatchProducer kafkaSettings batchWindowSize)

main :: IO ()
main = do
  cmd <- execParser (info commandParser idm)
  case cmd of
    ProduceEvery kafkaSettings n s -> runProducer kafkaSettings n s
    RunScheduler -> print ("someday" :: String)
