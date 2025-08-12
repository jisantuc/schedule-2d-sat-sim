{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.PubSub where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Aeson (eitherDecode)
import Data.Text (Text)
import Network.AMQP
  ( Ack (..),
    Message (..),
    ackEnv,
    bindQueue,
    declareExchange,
    declareQueue,
    exchangeName,
    exchangeType,
    newExchange,
    newQueue,
    openChannel,
    openConnection,
    queueExclusive,
    queueName,
  )
import Network.AMQP.Streamly (consume)
import SatSim.Schedulable (Schedulable)
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream

data RabbitMQConnectInfo = RabbitMQConnectInfo
  { host :: String,
    loginUser :: Text,
    auth :: Text,
    exchangeName' :: Text
  }
  deriving (Eq, Show)

consumeBatches ::
  ( MonadCatch m,
    MonadIO m,
    MonadIO (Stream m)
  ) =>
  ReaderT RabbitMQConnectInfo (Stream m) [Schedulable]
consumeBatches = do
  (RabbitMQConnectInfo {host, loginUser, auth, exchangeName'}) <- ask
  lift $ do
    conn <- liftIO $ openConnection host "/" loginUser auth
    chan <- liftIO $ openChannel conn
    liftIO $ declareExchange chan (newExchange {exchangeName = exchangeName', exchangeType = "fanout"})
    (queue, _, _) <- liftIO $ declareQueue chan (newQueue {queueName = "", queueExclusive = True})
    liftIO $ bindQueue chan queue exchangeName' ""
    consume chan queue Ack >>= \(msg, env) ->
      Stream.finallyIO (ackEnv env) $
        case eitherDecode (msgBody msg) of
          Right batch -> pure batch
          Left e -> Stream.fromEffect (liftIO $ print e) *> Stream.nil
