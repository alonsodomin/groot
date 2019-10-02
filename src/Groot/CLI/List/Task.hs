{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.Task
     ( ListTaskOpts
     , listTaskOpts
     , printTaskSummary
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Data
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS            as ECS
import           Options.Applicative
import           Text.PrettyPrint.Tabulate  (Tabulate, printTable)
import qualified Text.PrettyPrint.Tabulate  as Tabs

import           Groot.CLI.Common
import           Groot.CLI.List.Common
import           Groot.Console
import           Groot.Core
import           Groot.Types

data ListTaskOpts = ListTaskOpts (Maybe ClusterRef) (Maybe ContainerServiceRef)
  deriving (Eq, Show)

listTaskOpts :: Parser ListTaskOpts
listTaskOpts = ListTaskOpts
           <$> optional clusterOpt
           <*> optional containerServiceOpt

data TaskSummary = TaskSummary
  { taskId     :: String
  , taskDef    :: String
  , status     :: String
  , cluster    :: String
  , instanceId :: String
  , startedAt  :: String
  , stoppedAt  :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate TaskSummary Tabs.ExpandWhenNested

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

annotateTask :: MonadAWS m => ConduitT ECS.Task TaskAndRelatives m ()
annotateTask = CL.mapMaybeM (\t -> runMaybeT $
    (TR t) <$> taskInstance t
  )

summarizeTasks :: ListTaskOpts -> AWS [TaskSummary]
summarizeTasks opts = sourceToList $ taskSource opts .| annotateTask .| CL.mapMaybe summarize
  where taskSource (ListTaskOpts Nothing    Nothing)           = fetchAllTasks
        taskSource (ListTaskOpts (Just cid) Nothing)           = fetchTasks cid
        taskSource (ListTaskOpts cref       (Just serviceRef)) = fetchServiceTasks cref serviceRef

printTaskSummary :: ListTaskOpts -> GrootIO ()
printTaskSummary opts = useResource $ do
  desc <- awsResource $ summarizeTasks opts
  case desc of
    [] -> putWarn ("No tasks found" :: Text)
    xs -> liftIO $ printTable xs
