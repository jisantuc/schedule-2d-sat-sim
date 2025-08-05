module SatSim.PubSub where

import Data.Text (Text)
import Streamly.Data.Stream (Stream)
import SatSim.Schedulable (Schedulable)
import Control.Monad.Trans.Reader (ReaderT)

data RabbitMQConnectInfo = RabbitMQConnectInfo
  { host :: String,
    loginUser :: Text,
    auth :: Text,
    exchangeName' :: Text
  }
  deriving (Eq, Show)

consumeBatches :: ReaderT RabbitMQConnectInfo (Stream m) [Schedulable]
consumeBatches = undefined
