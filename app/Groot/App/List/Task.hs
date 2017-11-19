{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.App.List.Task
     ( printTaskSummary
     ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Data
import qualified Data.Text as T
import qualified Data.UUID as UUID
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

data TaskAndRelatives = TR ECS.Task ECS.TaskDefinition ECS.Cluster ECS.ContainerInstance

instance HasSummary TaskAndRelatives TaskSummary where
  summarize (TR t td c i) = TaskSummary <$> tId <*> tTaskDef <*> tStatus <*> tCluster <*> tInstanceId <*> tStartedAt <*> tStoppedAt
    where tFamily     = td ^. ECS.tdFamily
          tRevision   = toText <$> td ^. ECS.tdRevision
          tId         =
            let parsedArn = parseOnly parser <$> t ^. ECS.tTaskARN
                arn = join $ either (\_ -> Nothing) Just <$> parsedArn
            in (T.unpack . UUID.toText . view arnTaskId) <$> arn
          tTaskDef    = T.unpack <$> ((\x y -> T.concat [x, ":", y]) <$> tFamily <*> tRevision)
          tStatus     = T.unpack <$> t ^. ECS.tLastStatus
          tCluster    = T.unpack <$> c ^. ECS.cClusterName
          tInstanceId = T.unpack <$> i ^. ECS.ciEc2InstanceId
          tStartedAt  = pure $ maybe "" show $ t ^. ECS.tStartedAt
          tStoppedAt  = pure $ maybe "" show $ t ^. ECS.tStoppedAt

annotateTask :: MonadAWS m => Conduit ECS.Task m TaskAndRelatives
annotateTask = CL.mapMaybeM (\t -> runMaybeT $ 
    (TR t) <$> (taskDefFromTask t) <*> (taskCluster t) <*> (taskInstance t)
  )

summarizeTasks :: Maybe ClusterRef -> AWS [TaskSummary]
summarizeTasks mcid = sourceToList $ taskSource mcid =$= annotateTask =$= CL.mapMaybe summarize
  where taskSource Nothing    = fetchAllTasks
        taskSource (Just cid) = fetchTasks cid

printTaskSummary :: Maybe ClusterRef -> Env -> IO ()
printTaskSummary cId env = do
  xs <- runResourceT . runAWS env $ summarizeTasks cId
  printTable' "No tasks found" xs