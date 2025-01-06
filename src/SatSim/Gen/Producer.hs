{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SatSim.Gen.Producer where

import Conduit (sinkNull)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.Aeson (encode)
import Data.ByteString (toStrict)
import Data.Conduit (ConduitT, Void, await, yieldM, (.|))
import qualified Data.Conduit.Combinators as Conduit
import Data.Functor (void, (<&>))
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Kafka.Producer (ProducePartition (..), ProducerProperties, closeProducer, newProducer, produceMessage)
import Kafka.Producer.Types (ProducerRecord (..))
import qualified Kafka.Types as Kafka
import SatSim.Quantities (Radians (..), Seconds (..))
import SatSim.Schedulable (Schedulable (..))
import SatSim.TargetVector (mkTargetVector)
import System.Random

genSchedulable :: UTCTime -> Seconds -> Int -> IO [Schedulable]
genSchedulable startingAfter withinNextSeconds howMany =
  let generator = replicateM howMany $ do
        windowOpen <- randomRIO (1, withinNextSeconds) <&> ((`addUTCTime` startingAfter) . realToFrac)
        windowDuration <- realToFrac <$> randomRIO (Seconds 5, Seconds 30)
        targetVector <- mkTargetVector <$> randomIO <*> randomIO
        pure $
          Schedulable
            { vector = targetVector,
              startCollectBefore = addUTCTime windowDuration windowOpen,
              startCollectAfter = windowOpen,
              closeEnough = Radians pi / 4,
              arrivalOrder = 1
            }
   in generator <&> (\sched -> zipWith (\s idx -> s {arrivalOrder = idx}) sched [1 ..])

timeProducer :: Int -> ConduitT () UTCTime IO ()
timeProducer interval = Conduit.repeatM (threadDelay (interval * 1000000) *> getCurrentTime)

batchProducer :: Seconds -> ConduitT UTCTime [Schedulable] IO ()
batchProducer batchWindowSize =
  await >>= \case
    Just currentTime -> yieldM $ do
      howManyInBatch <- randomRIO (1, 20)
      genSchedulable currentTime batchWindowSize howManyInBatch
    Nothing -> fail "no more inputs"

stdoutBatchProducer :: Seconds -> ConduitT UTCTime Void IO ()
stdoutBatchProducer seconds = batchProducer seconds .| Conduit.mapM (print . show) .| sinkNull

kafkaBatchProducer :: ProducerProperties -> Seconds -> ConduitT UTCTime Void IO ()
kafkaBatchProducer props seconds =
  batchProducer seconds .| toProducerRecord .| Conduit.mapM produce .| sinkNull
  where
    toProducerRecord =
      Conduit.map
        ( \sched ->
            ProducerRecord
              { prValue = Just . toStrict . encode $ sched,
                prTopic = "schedulable-batches",
                prPartition = UnassignedPartition,
                prKey = Nothing,
                prHeaders = Kafka.headersFromList []
              }
        )

    produce message =
      bracket
        mkProducer
        clProducer
        ( \case
            Right producer -> void (produceMessage producer message)
            Left e -> print e
        )

    mkProducer = newProducer props

    clProducer (Left _) = return ()
    clProducer (Right prod) = void $ closeProducer prod
