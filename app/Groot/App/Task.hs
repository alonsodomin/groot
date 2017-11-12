module Groot.App.Task
     ( TaskOptions
     , grootTaskCli
     , runGrootTask
     ) where

import Data.Semigroup ((<>))
import Data.String
import Options.Applicative
import Network.AWS

import Groot.App.Cli.Parsers (clusterOpt)
import Groot.Core
import Groot.Data

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

grootTaskCli :: Parser TaskOptions
grootTaskCli = TaskOptions <$> taskCmds

runGrootTask :: TaskOptions -> Env -> IO ()
runGrootTask (TaskOptions cmd) env = runResourceT . runAWS env $ runCommand cmd
  where runCommand (StartTaskCmd   clusterRef taskRef) = startTask   taskRef clusterRef
        runCommand (StopTaskCmd    clusterRef taskRef) = stopTask    taskRef clusterRef
        runCommand (RestartTaskCmd clusterRef taskRef) = restartTask taskRef clusterRef