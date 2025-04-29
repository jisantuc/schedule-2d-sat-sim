{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)
import Database.Redis
  ( ConnectInfo (..),
    PortID (..),
    defaultConnectInfo,
    withCheckedConnect,
  )
import Network.AMQP (openChannel, openConnection)
import Options.Applicative
  ( Parser,
    auto,
    command,
    execParser,
    fullDesc,
    help,
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
import SatSim.Cache (RedisScheduleRepository (RedisScheduleRepository))
import SatSim.Consumer.AMQP (Heartbeat (..), consumeBatchesFromExchange)
import SatSim.Producer.AMQP (produceBatchesToExchange)
import SatSim.Quantities (Seconds (..))
import SatSim.Satellite (Satellite (..), SatelliteName (..))

data Command
  = RunScheduler
  | RabbitMQProducerDemo Text
  | RabbitMQConsumerDemo ConnectInfo Text (Heartbeat IO)

redisConnectInfoParser :: Parser ConnectInfo
redisConnectInfoParser =
  ( \host port dbNum auth ->
      ( defaultConnectInfo
          { connectHost = host,
            connectPort = port,
            connectDatabase = dbNum,
            connectAuth = auth
          }
      )
  )
    <$> option str (long "redis-host" <> value "localhost")
    <*> ( PortNumber
            <$> option auto (long "redis-port" <> value 6379)
        )
    <*> option auto (long "redis-database" <> value 0)
    <*> option auto (long "redis-password" <> value Nothing)

heartbeatParser :: (MonadIO m) => Parser (Heartbeat m)
heartbeatParser =
  Heartbeat (liftIO (print ("Consumer alive" :: String))) . Seconds
    <$> option
      auto
      ( long "heartbeat-interval"
          <> metavar "HEARTBEAT_INTERVAL"
          <> value 3
          <> help "Heartbeat interval seconds"
      )

amqpProducerDemoParser :: Parser Command
amqpProducerDemoParser = RabbitMQProducerDemo <$> option str (long "exchange-name" <> short 'x' <> metavar "EXCHANGE_NAME")

amqpConsumerDemoParser :: Parser Command
amqpConsumerDemoParser =
  RabbitMQConsumerDemo
    <$> redisConnectInfoParser
    <*> option str (long "exchange-name" <> short 'x' <> metavar "EXCHANGE_NAME")
    <*> heartbeatParser

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "amqp-producer-demo"
        (info amqpProducerDemoParser (fullDesc <> progDesc "produce to an amqp topic"))
        <> command
          "amqp-consumer-demo"
          (info amqpConsumerDemoParser (fullDesc <> progDesc "consume from an amqp topic"))
    )
    <**> helper

main :: IO ()
main = do
  cmd <- execParser (info commandParser idm)
  case cmd of
    RunScheduler -> print ("someday" :: String)
    RabbitMQProducerDemo exchangeName -> do
      conn <- openConnection "127.0.0.1" "/" "guest" "guest"
      chan <- openChannel conn
      runReaderT (produceBatchesToExchange 3 300 exchangeName) chan
    RabbitMQConsumerDemo connInfo exchangeName heartbeat ->
      withCheckedConnect
        connInfo
        ( runReaderT
            ( consumeBatchesFromExchange
                (SimpleSatellite 3 (SatelliteName "satellite"))
                exchangeName
                heartbeat
            )
            . RedisScheduleRepository
        )
