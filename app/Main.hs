{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Database.Redis
  ( ConnectInfo (..),
    PortID (..),
    defaultConnectInfo,
    withCheckedConnect,
  )
import Network.AMQP
  ( openChannel,
    openConnection,
  )
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
    str,
    value,
    (<**>),
  )
import SatSim.Cache (RedisScheduleRepository (RedisScheduleRepository))
import SatSim.Config (ConsumerConfig (..))
import SatSim.Core.Consumer (Heartbeat (..), consumeBatches)
import SatSim.Core.Producer (produceBatchesToExchange)
import SatSim.PubSub (RabbitMQConnectInfo (..))
import SatSim.Quantities (Seconds (..))
import SatSim.Satellite (Satellite (..), SatelliteName (..))

data Command
  = RunScheduler
  | RabbitMQProducerDemo RabbitMQConnectInfo
  | RabbitMQConsumerDemo ConnectInfo RabbitMQConnectInfo (Heartbeat IO)

rabbitMQConnectInfoParser :: Parser RabbitMQConnectInfo
rabbitMQConnectInfoParser =
  RabbitMQConnectInfo
    <$> option str (long "rabbitmq-host" <> value "localhost")
    <*> option str (long "rabbitmq-login-user" <> value "guest")
    <*> option str (long "rabbitmq-auth" <> value "guest")
    <*> option str (long "rabbitmq-exchange-name" <> value "batches")

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
amqpProducerDemoParser = RabbitMQProducerDemo <$> rabbitMQConnectInfoParser

amqpConsumerDemoParser :: Parser Command
amqpConsumerDemoParser =
  RabbitMQConsumerDemo
    <$> redisConnectInfoParser
    <*> rabbitMQConnectInfoParser
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
    RabbitMQProducerDemo (RabbitMQConnectInfo {host, loginUser, auth, exchangeName'}) -> do
      conn <- openConnection host "/" loginUser auth
      chan <- openChannel conn
      produceBatchesToExchange chan 3 300 exchangeName'
    RabbitMQConsumerDemo connInfo rabbitMQConnectInfo heartbeat ->
      withCheckedConnect
        connInfo
        ( runReaderT
            ( consumeBatches
                (SimpleSatellite 3 (SatelliteName "satellite"))
                heartbeat
            )
            . ConsumerConfig rabbitMQConnectInfo
            . RedisScheduleRepository
        )
