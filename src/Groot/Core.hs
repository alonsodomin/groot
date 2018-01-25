{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Groot.Core
     (
       module Groot.AWS
     , module Groot.Core.Common
     -- Error handlers
     , handleHttpException
     , handleServiceError
     ) where

import           Control.Lens
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Network.AWS
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import           Groot.AWS
import           Groot.Core.Common
import           Groot.Core.Console
import           Groot.Data.Text

-- AWS Error handlers

handleHttpException :: MonadConsole m => HttpException -> m ()
handleHttpException (InvalidUrlException url reason) =
  putError $ "Url " <> (styled yellowStyle $ toText url) <> " is invalid due to: " <> (styled redStyle $ toText reason)
handleHttpException (HttpExceptionRequest req _) =
  putError $ "Could not communicate with " <> (styled yellowStyle $ toText . host $ req) <> "."

handleServiceError :: MonadConsole m => ServiceError -> m ()
handleServiceError err =
  let servName  = toText $ err ^. serviceAbbrev
      statusMsg = toText . statusMessage $ err ^. serviceStatus
      message   = maybe "" toText $ err ^. serviceMessage
      styledSt  = styled yellowStyle (T.concat [servName, " ", statusMsg])
  in putError $ styledSt <+> (styleless message)

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
