{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module SatSim.Core.BatchStreamSource where

import Control.Monad.Trans.Reader (ReaderT)
import SatSim.PubSub (RabbitMQConnectInfo, consumeBatches)
import SatSim.Schedulable (Schedulable)
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream

class BatchStreamSource m where
  batches :: Stream m [Schedulable]

instance
  (Monad (Stream m0)) =>
  BatchStreamSource (ReaderT RabbitMQConnectInfo (Stream m0))
  where
  batches = Stream.fromEffect consumeBatches
