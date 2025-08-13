{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SatSim.Core.BatchStreamSource where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import SatSim.PubSub (RabbitMQConnectInfo, consumeBatches)
import SatSim.Schedulable (Schedulable)
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream

newtype StaticBatchStream = StaticBatchStream {batchesToEmit :: [[Schedulable]]} deriving (Eq, Show)

class BatchStreamSource m where
  batches :: Stream m [Schedulable]

instance (MonadIO m, MonadCatch m) => BatchStreamSource (ReaderT RabbitMQConnectInfo m) where
  batches = consumeBatches

instance BatchStreamSource (ReaderT StaticBatchStream IO) where
  batches =
    Stream.concatMap
      (Stream.fromList . batchesToEmit)
      (Stream.fromEffect ask)
