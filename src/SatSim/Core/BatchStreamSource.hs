{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SatSim.Core.BatchStreamSource where

import Control.Monad.Trans.Reader (ReaderT)
import SatSim.PubSub (RabbitMQConnectInfo, consumeBatches)
import SatSim.Schedulable (Schedulable)
import Streamly.Data.Stream (Stream)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)

class BatchStreamSource m where
  batches :: Stream m [Schedulable]

instance (MonadIO m, MonadCatch m) => BatchStreamSource (ReaderT RabbitMQConnectInfo m)
  where
  batches = consumeBatches
