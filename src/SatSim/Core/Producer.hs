{-# LANGUAGE OverloadedStrings #-}

module SatSim.Core.Producer where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (encode)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.AMQP
  ( Channel,
    DeliveryMode (..),
    ExchangeOpts (..),
    Message (..),
    declareExchange,
    newExchange,
    newMsg,
    publishMsg,
  )
import SatSim.Gen.Producer (genSchedulable)
import SatSim.Quantities (Seconds (..))
import System.Random (randomRIO)

-- | Produce a batch of schedule candidates to an exchange on the default (local) RabbitMQ host
produceBatchesToExchange :: (MonadIO m) => Channel -> Seconds -> Seconds -> Text -> m ()
produceBatchesToExchange chan (Seconds awakeEveryMs) batchWindowSize exchangeName' =
  forever . liftIO $
    threadDelay (floor awakeEveryMs * 1000000) >> do
      currentTime <- getCurrentTime
      batchSize <- randomRIO (1, 20)
      candidates <- genSchedulable currentTime batchWindowSize batchSize
      declareExchange chan (newExchange {exchangeName = exchangeName', exchangeType = "fanout"})
      print $ "Publishing message with length: " <> show (length candidates)
      void $
        publishMsg
          chan
          exchangeName'
          "otherkey"
          ( newMsg
              { msgBody = encode candidates,
                msgDeliveryMode = Just Persistent
              }
          )
