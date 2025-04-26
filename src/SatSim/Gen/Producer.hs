module SatSim.Gen.Producer where

import Control.Monad (replicateM)
import Data.Functor ((<&>))
import Data.Time (UTCTime, addUTCTime)
import SatSim.Quantities (Radians (..), Seconds (..))
import SatSim.Schedulable (Schedulable (..))
import SatSim.TargetVector (mkTargetVector)
import System.Random (randomRIO)

genSchedulable :: UTCTime -> Seconds -> Int -> IO [Schedulable]
genSchedulable startingAfter withinNextSeconds howMany =
  let generator = replicateM howMany $ do
        windowOpen <- randomRIO (1, withinNextSeconds) <&> ((`addUTCTime` startingAfter) . realToFrac)
        windowDuration <- realToFrac <$> randomRIO (Seconds 5, Seconds 30)
        targetVector <- mkTargetVector <$> randomRIO (-1, 1) <*> randomRIO (-1, 1)
        pure $
          Schedulable
            { vector = targetVector,
              startCollectBefore = addUTCTime windowDuration windowOpen,
              startCollectAfter = windowOpen,
              closeEnough = Radians pi / 4,
              arrivalOrder = 1
            }
   in generator <&> (\sched -> zipWith (\s idx -> s {arrivalOrder = idx}) sched [1 ..])
