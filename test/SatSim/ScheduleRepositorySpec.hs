module SatSim.ScheduleRepositorySpec where

import Control.Concurrent (newMVar)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (traverse_)
import qualified Data.IntervalIndex as IntervalIndex
import Data.These.Combinators (justThat)
import Data.Time (UTCTime (UTCTime), addUTCTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import SatSim.Algo.Greedy (scheduleOn)
import SatSim.Cache (LocalScheduleRepository (..))
import SatSim.Quantities (Radians (..))
import SatSim.Satellite (Satellite (..), SatelliteName (..))
import SatSim.Schedulable (Schedulable (..), ScheduleId (..))
import SatSim.ScheduleRepository (readSchedule, writeSchedule)
import SatSim.TargetVector (mkTargetVector)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  let candidate =
        Schedulable
          { vector = point,
            startCollectBefore = endTime,
            startCollectAfter = startTime,
            closeEnough = Radians 1,
            arrivalOrder = 1
          }
      fastSatellite = SimpleSatellite (Radians 2) (SatelliteName "fast")
      startTime = UTCTime (fromOrdinalDate 2024 304) 2934
      endTime = addUTCTime 100000 startTime
      point = mkTargetVector 1 3
      exampleSchedule =
        justThat $
          scheduleOn
            fastSatellite
            [candidate]
            IntervalIndex.empty
   in describe "ScheduleRepositorySpec" $ do
        it "reads after write" $ do
          mvar <- newMVar mempty
          raw <-
            runReaderT
              ( do
                  traverse_ (writeSchedule (ScheduleId "test")) exampleSchedule
                  readSchedule (ScheduleId "test")
              )
              (LocalScheduleRepository mvar)
          raw `shouldBe` exampleSchedule
