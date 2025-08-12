{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.PubSub (consumeBatches, RabbitMQConnectInfo(..)) where

import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Aeson (eitherDecode)
import Data.Functor.Alt (($>))
import Data.Text (Text)
import Network.AMQP
  ( Ack (..),
    Message (..),
    ackEnv,
    bindQueue,
    closeConnection,
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

consumeBatches :: Stream (ReaderT RabbitMQConnectInfo IO) [Schedulable]
consumeBatches =
  let release (_, _, conn) = closeConnection conn
      acquire host loginUser auth exchangeName' =
        ( do
            conn <- openConnection host "/" loginUser auth
            chan <- openChannel conn
            declareExchange chan (newExchange {exchangeName = exchangeName', exchangeType = "fanout"})
            (queue, _, _) <- declareQueue chan (newQueue {queueName = "", queueExclusive = True})
            bindQueue chan queue exchangeName' ""
            pure (chan, queue, conn)
        )
      emitParsed (msg, env) =
        Stream.finallyIO (ackEnv env) $
          case eitherDecode (msgBody msg) of
            Right batch -> Stream.fromPure batch
            Left e -> Stream.liftInner (Stream.fromEffect (print e) $> [])
      consumeBatch (chan, queue, _) = Stream.concatMap emitParsed (consume chan queue Ack)
   in Stream.concatMap
        ( \(RabbitMQConnectInfo {host, loginUser, auth, exchangeName'}) ->
            Stream.bracketIO
              (acquire host loginUser auth exchangeName')
              release
              consumeBatch
        )
        (Stream.fromEffect ask)
