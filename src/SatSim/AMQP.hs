{-# LANGUAGE OverloadedStrings #-}

module SatSim.AMQP where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Functor (void)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.AMQP
  ( Ack (..),
    DeliveryMode (Persistent),
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
    newMsg,
    newQueue,
    openChannel,
    openConnection,
    publishMsg,
  )
import SatSim.Gen.Producer (genSchedulable)
import SatSim.Quantities (Seconds (..))
import SatSim.Schedulable (Schedulable (..))
import System.Random (randomRIO)

myCallback :: (Message, Envelope) -> IO ()
myCallback (msg, env) = do
  putStrLn $ "received message: " ++ BL.unpack (msgBody msg)
  -- acknowledge receiving the message
  ackEnv env

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

consumeBatchesFromExchange :: Text -> IO ()
consumeBatchesFromExchange exchangeName' = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn
  declareExchange chan (newExchange {exchangeName = exchangeName', exchangeType = "fanout"})
  (queue, _, _) <- declareQueue chan (newQueue {queueName = "", queueExclusive = True})
  bindQueue chan queue exchangeName' "otherkey"
  void $ consumeMsgs chan queue Ack callback

  -- TODO: factor to `heartbeat` of some sort
  -- In a more realistic application, this would be some kind of heartbeat mechanism
  void $ forever (threadDelay (30 * 1000000) *> putStrLn "Still alive")
  closeConnection conn
  where
    callback :: (Message, Envelope) -> IO ()
    callback (msg, envelope) = do
      either putStrLn (print . length) (eitherDecode (msgBody msg) :: Either String [Schedulable])
      ackEnv envelope

amqpDemo :: Text -> IO ()
amqpDemo xName =
  do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
    declareExchange chan (newExchange {exchangeName = xName, exchangeType = "fanout"})
    -- declare a queue, exchange and binding
    (queue, _, _) <- declareQueue chan (newQueue {queueName = "", queueExclusive = True})
    bindQueue chan queue xName ""

    -- subscribe to the queue
    void $ consumeMsgs chan queue Ack myCallback

    -- publish a message to our new exchange
    void $
      publishMsg
        chan
        xName
        ""
        ( newMsg
            { msgBody = BL.pack "hello world",
              msgDeliveryMode = Just Persistent
            }
        )

    void getLine -- wait for keypress
    closeConnection conn
    putStrLn "connection closed"

demo :: IO ()
demo = undefined
