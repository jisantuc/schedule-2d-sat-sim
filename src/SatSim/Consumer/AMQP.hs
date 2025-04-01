{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.Consumer.AMQP where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecode)
import Data.Functor (void)
import Data.Text (Text)
import Network.AMQP
  ( Ack (..),
    Envelope,
    ExchangeOpts (..),
    Message (..),
    QueueOpts (..),
    ackEnv,
    bindQueue,
    closeConnection,
    consumeMsgs,
    declareExchange,
    declareQueue,
    newExchange,
    newQueue,
    openChannel,
    openConnection,
  )
import SatSim.Quantities (Seconds (..))
import SatSim.Schedulable (Schedulable (..))

data Heartbeat m = Heartbeat {unheartbeat :: m (), interval :: Seconds}

beatForever :: (MonadIO m) => Heartbeat m -> m ()
beatForever (Heartbeat {unheartbeat, interval}) =
  void . forever $
    liftIO (threadDelay (floor interval * 1000000)) *> unheartbeat

consumeBatchesFromExchange :: Text -> Heartbeat IO -> IO ()
consumeBatchesFromExchange exchangeName' hb = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn
  declareExchange chan (newExchange {exchangeName = exchangeName', exchangeType = "fanout"})
  (queue, _, _) <- declareQueue chan (newQueue {queueName = "", queueExclusive = True})
  bindQueue chan queue exchangeName' "otherkey"
  void $ consumeMsgs chan queue Ack callback

  beatForever hb

  closeConnection conn
  where
    callback :: (Message, Envelope) -> IO ()
    callback (msg, envelope) = do
      case (eitherDecode (msgBody msg) :: Either String [Schedulable]) of
        -- TODO: need a satellite from somewhere? and a redis connection
        Right b -> print . length $ b
        Left e -> putStrLn e
      either putStrLn (print . length) (eitherDecode (msgBody msg) :: Either String [Schedulable])
      ackEnv envelope
