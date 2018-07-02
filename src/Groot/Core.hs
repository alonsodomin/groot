module Groot.Core
     (
       module Groot.Internal.AWS
     , module Groot.Core.Common
     ) where

import           Groot.Core.Common
import           Groot.Internal.AWS

-- Tasks

-- stopTask :: MonadAWS m
--          => TaskRef
--          -> ClusterRef
--          -> (TaskRef -> ClusterRef -> m ())
--          -> (TaskRef -> ClusterRef -> m ())
--          -> m ()
-- stopTask tref@(TaskRef taskRef) clusterRef onStop onStopped =
--   let describeReq = dtCluster ?~ (toText clusterRef) $ dtTasks .~ [taskRef] $ describeTasks
--   in do
--     send $ stCluster ?~ (toText clusterRef) $ ECS.stopTask taskRef
--     onStop tref clusterRef
--     result <- A.await tasksStopped describeReq
--     case result of
--       AcceptSuccess -> onStopped tref clusterRef
--       _             -> throwM $ taskStatusTransitionFailed tref TSRunning TSStopped

-- startTask :: MonadAWS m
--           => TaskRef
--           -> ClusterRef
--           -> (TaskRef -> ClusterRef -> m ())
--           -> (TaskRef -> ClusterRef -> m ())
--           -> m ()
-- startTask tref@(TaskRef taskRef) clusterRef onStart onStarted =
--   let describeReq = dtCluster ?~ (toText clusterRef) $ dtTasks .~ [taskRef] $ describeTasks
--   in do
--     send $ sCluster ?~ (toText clusterRef) $ ECS.startTask taskRef
--     onStart tref clusterRef
--     result <- A.await tasksRunning describeReq
--     case result of
--       AcceptSuccess -> onStarted tref clusterRef
--       _             -> throwM $ taskStatusTransitionFailed tref TSStopped TSRunning
