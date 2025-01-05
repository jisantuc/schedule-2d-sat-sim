module Main where

import Control.Monad (forever)
import Data.Conduit (runConduit, (.|))
import Options.Applicative
  ( Parser,
    auto,
    command,
    execParser,
    fullDesc,
    helper,
    idm,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    subparser,
    (<**>),
  )
import SatSim.Gen.Producer (stdoutBatchProducer, timeProducer)
import SatSim.Quantities (Seconds (..))

data Command
  = ProduceEvery Int Seconds
  | RunScheduler

produceEvery :: Parser Command
produceEvery =
  ProduceEvery
    <$> option auto (long "time-between-batches" <> short 't' <> metavar "BATCH_INTERVAL")
    <*> (Seconds <$> option auto (long "batch-window-size" <> short 'w' <> metavar "BATCH_INTERVAL"))

commandParser :: Parser Command
commandParser =
  subparser
    ( command "producer" $
        info
          (produceEvery <**> helper)
          ( fullDesc
              <> progDesc "Produce a batch of schedulable tasks every BATCH_INTERVAL seconds"
          )
    )

runProducer :: Int -> Seconds -> IO ()
runProducer timeBetweenBatches batchWindowSize =
  forever . runConduit $ (timeProducer timeBetweenBatches .| stdoutBatchProducer batchWindowSize)

main :: IO ()
main = do
  cmd <- execParser (info commandParser idm)
  case cmd of
    ProduceEvery n s -> runProducer n s
    RunScheduler -> print "someday"
