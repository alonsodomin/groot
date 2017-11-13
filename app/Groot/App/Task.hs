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
import System.IO

import Groot.App.Cli.Parsers (clusterOpt)
import Groot.App.Console
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
  liftIO $ do
    putStr $ "Starting task '" ++ (T.unpack . toText $ taskRef) 
      ++ "' in cluster '" ++ (T.unpack . toText $ clusterRef) ++ "'... "
    hFlush stdout

stoppingTask :: MonadAWS m => TaskRef -> ClusterRef -> m ()
stoppingTask taskRef clusterRef =
  liftIO $ do
    putStr $ "Stopping task '" ++ (T.unpack . toText $ taskRef) 
      ++ "' in cluster '" ++ (T.unpack . toText $ clusterRef) ++ "'... "
    hFlush stdout

succeeded :: MonadAWS m => TaskRef -> ClusterRef -> m ()
succeeded _ _ = liftIO $ do
  setSGR [SetColor Foreground Dull Green]
  putStrLn "OK"
  setSGR [Reset]

failed :: TaskStatusTransitionFailed -> IO ()
failed _ = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn "FAILED"
  setSGR [Reset]

grootTaskCli :: Parser TaskOptions
grootTaskCli = TaskOptions <$> taskCmds

stopTaskPrompt :: TaskRef -> ClusterRef -> String
stopTaskPrompt taskRef clusterRef = concat [
    "This will stop task '"
  , T.unpack . toText $ taskRef
  , "' in cluster '"
  , T.unpack . toText $ clusterRef
  , "'. Continue?"
  ]

restartTaskPrompt :: TaskRef -> ClusterRef -> String
restartTaskPrompt taskRef clusterRef = concat [
    "This will restart task '"
  , T.unpack . toText $ taskRef
  , "' in cluster '"
  , T.unpack . toText $ clusterRef
  , "'. Continue?"
  ]

startTask' :: MonadAWS m => TaskRef -> ClusterRef -> m ()
startTask' taskRef clusterRef = do
  getTask taskRef (Just clusterRef)
  startTask taskRef clusterRef startingTask succeeded

stopTask' :: MonadAWS m => TaskRef -> ClusterRef -> m ()
stopTask' taskRef clusterRef =
  let prompt = stopTaskPrompt taskRef clusterRef
  in do
    getTask taskRef (Just clusterRef)
    promptUserToContinue prompt (stopTask taskRef clusterRef stoppingTask succeeded)

restartTask' :: MonadAWS m => TaskRef -> ClusterRef -> m ()
restartTask' taskRef clusterRef =
  let prompt = restartTaskPrompt taskRef clusterRef
      stopAndStart = do
        stopTask taskRef clusterRef stoppingTask succeeded
        startTask taskRef clusterRef startingTask succeeded
  in do
    getTask taskRef (Just clusterRef)
    promptUserToContinue prompt stopAndStart

runGrootTask :: TaskOptions -> Env -> IO ()
runGrootTask (TaskOptions cmd) env =
  catching _TaskStatusTransitionFailed (runResourceT . runAWS env $ runCommand cmd) failed
  where runCommand (StartTaskCmd   clusterRef taskRef) = startTask'   taskRef clusterRef
        runCommand (StopTaskCmd    clusterRef taskRef) = stopTask'    taskRef clusterRef
        runCommand (RestartTaskCmd clusterRef taskRef) = restartTask' taskRef clusterRef