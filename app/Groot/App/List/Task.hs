{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.App.List.Task
     ( printTaskSummary
     ) where

import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Data
import qualified Data.Text as T
import GHC.Generics
import Text.PrettyPrint.Tabulate
import Network.AWS
import qualified Network.AWS.ECS as ECS

import Groot.App.List.Base
import Groot.Core
import Groot.Data
import Groot.Data.Text
import Groot.Types

data TaskSummary = TaskSummary
  { taskId     :: String
  , task       :: String
  , status     :: String
  , cluster    :: String
  , instanceId :: String
  , startedAt  :: String
  , stoppedAt  :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate TaskSummary

data TaskAndRelatives = TR ECS.Task ECS.ContainerInstance

instance HasSummary TaskAndRelatives TaskSummary where
  summarize (TR t i) = TaskSummary <$> tId <*> tTaskDef <*> tStatus <*> tCluster <*> tInstanceId <*> tStartedAt <*> tStoppedAt
    where tId         = (asString . view arnTaskId) <$> viewArn (ECS.tTaskARN . _Just) t
          tTaskDef    = (asString . view arnTaskDefId) <$> viewArn (ECS.tTaskDefinitionARN . _Just) t
          tStatus     = T.unpack <$> t ^. ECS.tLastStatus
          tCluster    = (asString . view arnClusterName) <$> viewArn (ECS.tClusterARN . _Just) t
          tInstanceId = T.unpack <$> i ^. ECS.ciEc2InstanceId
          tStartedAt  = pure $ maybe "" show $ t ^. ECS.tStartedAt
          tStoppedAt  = pure $ maybe "" show $ t ^. ECS.tStoppedAt

annotateTask :: MonadAWS m => Conduit ECS.Task m TaskAndRelatives
annotateTask = CL.mapMaybeM (\t -> runMaybeT $ 
    (TR t) <$> taskInstance t
  )

summarizeTasks :: Maybe ClusterRef -> AWS [TaskSummary]
summarizeTasks mcid = sourceToList $ taskSource mcid =$= annotateTask =$= CL.mapMaybe summarize
  where taskSource Nothing    = fetchAllTasks
        taskSource (Just cid) = fetchTasks cid

printTaskSummary :: Maybe ClusterRef -> Env -> IO ()
printTaskSummary cId env = do
  xs <- runResourceT . runAWS env $ summarizeTasks cId
  printTable' "No tasks found" xs