{-# LANGUAGE RankNTypes #-}

module Groot.Core 
     ( 
       module Groot.AWS
     -- Tasks
     , stopTask
     , startTask
     , restartTask
     ) where

import Control.Lens
import Control.Monad.Catch
import Network.AWS hiding (await)
import qualified Network.AWS as A
import Network.AWS.Data.Text
import Network.AWS.ECS hiding (cluster, stopTask, startTask, Running, Stopped)
import qualified Network.AWS.ECS as ECS
import Network.AWS.Waiter

import Groot.AWS
import Groot.Data
import Groot.Exception

-- Tasks

stopTask :: MonadAWS m
         => TaskRef
         -> ClusterRef
         -> (TaskRef -> ClusterRef -> m ())
         -> (TaskRef -> ClusterRef -> m ())
         -> m ()
stopTask tref@(TaskRef taskRef) clusterRef onStop onStopped = 
  let describeReq = dtCluster ?~ (toText clusterRef) $ dtTasks .~ [taskRef] $ describeTasks
  in do
    send $ stCluster ?~ (toText clusterRef) $ ECS.stopTask taskRef
    onStop tref clusterRef
    result <- A.await tasksStopped describeReq
    case result of
      AcceptSuccess -> onStopped tref clusterRef
      _             -> throwM $ taskStatusTransitionFailed tref Running Stopped

startTask :: MonadAWS m 
          => TaskRef
          -> ClusterRef
          -> (TaskRef -> ClusterRef -> m ())
          -> (TaskRef -> ClusterRef -> m ())
          -> m ()
startTask tref@(TaskRef taskRef) clusterRef onStart onStarted = 
  let describeReq = dtCluster ?~ (toText clusterRef) $ dtTasks .~ [taskRef] $ describeTasks
  in do
    send $ sCluster ?~ (toText clusterRef) $ ECS.startTask taskRef
    onStart tref clusterRef
    result <- A.await tasksRunning describeReq
    case result of
      AcceptSuccess -> onStarted tref clusterRef
      _             -> throwM $ taskStatusTransitionFailed tref Stopped Running

restartTask :: MonadAWS m
            => TaskRef
            -> ClusterRef
            -> (TaskRef -> ClusterRef -> m ())
            -> (TaskRef -> ClusterRef -> m ())
            -> (TaskRef -> ClusterRef -> m ())
            -> (TaskRef -> ClusterRef -> m ())
            -> m ()
restartTask taskRef clusterRef onStop onStopped onStart onStarted = do
  stopTask taskRef clusterRef onStop onStopped
  startTask taskRef clusterRef onStart onStarted