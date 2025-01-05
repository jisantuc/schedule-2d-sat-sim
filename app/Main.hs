{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Lens ((.~))
import Control.Monad (forever)
import Data.Aeson (ToJSON (toJSON), Value (Object))
import Data.Aeson.KeyMap (fromList)
import Data.Conduit (runConduit, (.|))
import Data.Foldable (traverse_)
import Data.Function ((&))
import Network.Wreq (defaults, header, putWith)
import Options.Applicative
  ( Parser,
    auto,
    command,
    execParser,
    fullDesc,
    helper,
    idm,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    subparser,
    (<**>),
  )
import SatSim.Gen.Producer (stdoutBatchProducer, timeProducer)
import SatSim.Quantities (Seconds (..))
import System.Environment (lookupEnv)

data Command
  = ProduceEvery Int Seconds
  | RunScheduler
  | SetupKafka

setupKafka :: IO ()
setupKafka =
  let topics = ["schedulable-batches"] :: [String]
      opts =
        defaults
          & header "Content-Type" .~ ["application/json"]
          & header "Accept" .~ ["application/json"]
      apiBase clusterId =
        "http://localhost:9021/2.0/kafka/" <> clusterId <> "/topics?validate=false"
      createTopic api topic =
        putWith
          opts
          api
          ( Object . fromList $ [("name", toJSON topic), ("numPartitions", "1"), ("replicationFactor", "1")]
          )
   in do
        clusterId <- lookupEnv "CLUSTER_ID"
        case clusterId of
          Just cluster ->
            traverse_ (createTopic (apiBase cluster)) topics
          Nothing -> fail "CLUSTER_ID is a required env var"

produceEvery :: Parser Command
produceEvery =
  ProduceEvery
    <$> option auto (long "time-between-batches" <> short 't' <> metavar "BATCH_INTERVAL")
    <*> (Seconds <$> option auto (long "batch-window-size" <> short 'w' <> metavar "BATCH_INTERVAL"))

commandParser :: Parser Command
commandParser =
  subparser
    ( command "producer" $
        info
          (produceEvery <**> helper)
          ( fullDesc
              <> progDesc "Produce a batch of schedulable tasks every BATCH_INTERVAL seconds"
          )
    )
    <|> subparser
      ( command "setup-kafka" $
          info (pure SetupKafka <**> helper) (fullDesc <> progDesc "Configure required Kafka topics")
      )

runProducer :: Int -> Seconds -> IO ()
runProducer timeBetweenBatches batchWindowSize =
  forever . runConduit $ (timeProducer timeBetweenBatches .| stdoutBatchProducer batchWindowSize)

main :: IO ()
main = do
  cmd <- execParser (info commandParser idm)
  case cmd of
    ProduceEvery n s -> runProducer n s
    RunScheduler -> print ("someday" :: String)
    SetupKafka -> setupKafka
