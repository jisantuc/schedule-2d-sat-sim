{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Conduit (runConduit, (.|))
import Data.List (singleton)
import Kafka.Producer
  ( BrokerAddress (BrokerAddress),
    ProducerProperties (ppKafkaProps, ppTopicProps),
    Timeout (Timeout),
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
    value,
    (<**>),
  )
import SatSim.Gen.Producer (kafkaBatchProducer, timeProducer)
import SatSim.Quantities (Seconds (..))
import Data.Text (pack)

data Command
  = ProduceEvery ProducerProperties Int Seconds
  | RunScheduler

-- TODO: something's busted with the broker parser, and I'm not getting any info about what.
-- add a debug command that just does execParser with some pretty printing at different parse levels
producerPropertiesParser :: Parser ProducerProperties
producerPropertiesParser =
  let brokers =
        brokersList . singleton . BrokerAddress . pack
          <$> option
            auto
            ( long "default-broker"
                <> metavar "DEFAULT_BROKER"
                <> value "localhost:9092"
            )
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
    ProduceEvery kafkaSettings n s -> do
      print (ppKafkaProps kafkaSettings)
      print (ppTopicProps kafkaSettings)
      runProducer kafkaSettings n s
    RunScheduler -> print ("someday" :: String)
