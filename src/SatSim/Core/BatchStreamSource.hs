{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SatSim.Core.BatchStreamSource where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import SatSim.PubSub (RabbitMQConnectInfo, consumeBatches)
import SatSim.Schedulable (Schedulable)
import Streamly.Data.Stream (Stream)

class BatchStreamSource t m where
  batches :: t (Stream m) [Schedulable]

instance
  (MonadCatch m, Monad (Stream m), MonadIO m, MonadIO (Stream m)) =>
  BatchStreamSource (ReaderT RabbitMQConnectInfo) m
  where
  batches = consumeBatches
