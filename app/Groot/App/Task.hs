module Groot.App.Task
     ( TaskOptions
     , grootTaskCli
     , runGrootTask
     ) where

import Control.Monad.IO.Class
import Data.Semigroup ((<>))
import Data.String
import qualified Data.Text as T
import Options.Applicative
import Network.AWS
import Network.AWS.Data.Text
import System.Console.ANSI

import Groot.App.Cli.Parsers (clusterOpt)
import Groot.Core
import Groot.Data
import Groot.Exception

data TaskCmd =
    StartTaskCmd ClusterRef TaskRef
  | StopTaskCmd ClusterRef TaskRef
  | RestartTaskCmd ClusterRef TaskRef
  deriving (Eq, Show)

data TaskOptions = TaskOptions TaskCmd
  deriving (Eq, Show)

-- CLI

taskRefArg :: Parser TaskRef
taskRefArg = (fromString <$> argument str (metavar "TASK"))

startTaskCli :: Parser TaskCmd
startTaskCli = StartTaskCmd
           <$> clusterOpt
           <*> taskRefArg

stopTaskCli :: Parser TaskCmd
stopTaskCli = StopTaskCmd
          <$> clusterOpt
          <*> taskRefArg

restartTaskCli :: Parser TaskCmd
restartTaskCli = RestartTaskCmd
             <$> clusterOpt
             <*> taskRefArg

taskCmds :: Parser TaskCmd
taskCmds = hsubparser
  ( command "start"   (info startTaskCli   (progDesc "Start task"))
 <> command "stop"    (info stopTaskCli    (progDesc "Stop task"))
 <> command "restart" (info restartTaskCli (progDesc "Restart task"))
  )

startingTask :: MonadAWS m => TaskRef -> ClusterRef -> m ()
startingTask taskRef clusterRef =
  liftIO . putStr $ "Starting task '" ++ (T.unpack . toText $ taskRef) 
    ++ "' in cluster '" ++ (T.unpack . toText $ clusterRef) ++ "'... "

stoppingTask :: MonadAWS m => TaskRef -> ClusterRef -> m ()
stoppingTask taskRef clusterRef =
  liftIO . putStr $ "Stopping task '" ++ (T.unpack . toText $ taskRef) 
    ++ "' in cluster '" ++ (T.unpack . toText $ clusterRef) ++ "'... "

succeeded :: MonadAWS m => TaskRef -> ClusterRef -> m ()
succeeded _ _ = liftIO $ do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "OK"
  setSGR [Reset]

failed :: TaskStatusTransitionFailed -> IO ()
failed _ = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn "FAILED"
  setSGR [Reset]

grootTaskCli :: Parser TaskOptions
grootTaskCli = TaskOptions <$> taskCmds

runGrootTask :: TaskOptions -> Env -> IO ()
runGrootTask (TaskOptions cmd) env =
  catching _TaskStatusTransitionFailed (runResourceT . runAWS env $ runCommand cmd) failed
  where runCommand (StartTaskCmd   clusterRef taskRef) =
          startTask taskRef clusterRef startingTask succeeded
        runCommand (StopTaskCmd    clusterRef taskRef) =
          stopTask taskRef clusterRef stoppingTask succeeded
        runCommand (RestartTaskCmd clusterRef taskRef) = do
          stopTask taskRef clusterRef stoppingTask succeeded
          startTask taskRef clusterRef startingTask succeeded