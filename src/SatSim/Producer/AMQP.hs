{-# LANGUAGE OverloadedStrings #-}

module SatSim.Producer.AMQP where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Data.Aeson (encode)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.AMQP
  ( DeliveryMode (..),
    ExchangeOpts (..),
    Message (..),
    declareExchange,
    newExchange,
    newMsg,
    openChannel,
    openConnection,
    publishMsg,
  )
import SatSim.Gen.Producer (genSchedulable)
import SatSim.Quantities (Seconds (..))
import System.Random (randomRIO)

-- | Produce a batch of schedule candidates to an exchange on the default (local) RabbitMQ host
produceBatchesToExchange :: Seconds -> Seconds -> Text -> IO ()
produceBatchesToExchange (Seconds awakeEveryMs) batchWindowSize exchangeName' =
  forever $ do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
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
