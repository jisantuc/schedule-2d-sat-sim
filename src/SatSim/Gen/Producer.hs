{-# LANGUAGE LambdaCase #-}

module SatSim.Gen.Producer where

import Conduit (sinkNull)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Data.Conduit (ConduitT, Void, await, yieldM, (.|))
import qualified Data.Conduit.Combinators as Conduit
import Data.Functor ((<&>))
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
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
