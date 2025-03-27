{-# LANGUAGE OverloadedStrings #-}

module SatSim.AMQP where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Functor (void)
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

myCallback :: (Message, Envelope) -> IO ()
myCallback (msg, env) = do
  putStrLn $ "received message: " ++ BL.unpack (msgBody msg)
  -- acknowledge receiving the message
  ackEnv env

amqpDemo :: IO ()
amqpDemo =
  let xName = "demo-exchange"
   in do
        conn <- openConnection "127.0.0.1" "/" "guest" "guest"
        chan <- openChannel conn
        -- declare a queue, exchange and binding
        (queue, _, _) <- declareQueue chan (newQueue {queueName = "lol", queueDurable = True})
        print $ "Queue is: " <> show queue
        declareExchange chan newExchange {exchangeName = xName, exchangeType = "direct"}
        bindQueue chan queue xName "myKey"

        -- subscribe to the queue
        void $ consumeMsgs chan queue Ack myCallback

        -- publish a message to our new exchange
        void $
          publishMsg
            chan
            xName
            "myKey"
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
