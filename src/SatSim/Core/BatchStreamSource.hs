{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SatSim.Core.BatchStreamSource where

import Control.Monad.Trans.Reader (ReaderT)
import SatSim.PubSub (RabbitMQConnectInfo, consumeBatches)
import SatSim.Schedulable (Schedulable)
import Streamly.Data.Stream (Stream)

class BatchStreamSource m where
  batches :: Stream m [Schedulable]

instance BatchStreamSource (ReaderT RabbitMQConnectInfo IO)
  where
  batches = consumeBatches
