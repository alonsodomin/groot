{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Groot.App.List.Cluster
     ( printClusterSummary
     ) where

import Data.Conduit
import Network.AWS
import qualified Network.AWS.ECS as ECS

import Groot.Core
import Groot.Data.Text
import Groot.Data.Text.Lens
import Groot.Data.Text.PrettyPrint

data ClusterAttr =
    CAName
  | CAStatus
  | CARunningTasks
  | CAPendingTasks
  | CAInstanceCount
  deriving (Eq, Show, Enum, Bounded, Ord)

instance PrettyColumn ClusterAttr where
  type PrettyItemOf ClusterAttr = ECS.Cluster

  columnHeader CAName          = "NAME"
  columnHeader CAStatus        = "STATUS"
  columnHeader CARunningTasks  = "RUNNING TASKS"
  columnHeader CAPendingTasks  = "PENDING TASKS"
  columnHeader CAInstanceCount = "# INSTANCES"

  -- columnStyle CAStatus "ACTIVE"   = color Dull Green
  -- columnStyle CAStatus "INACTIVE" = color Vivid Red
  -- columnStyle _        _          = mempty

  columnCell CAName          = ECS.cClusterName . orEmpty
  columnCell CAStatus        = ECS.cStatus . orEmpty
  columnCell CARunningTasks  = ECS.cRunningTasksCount . orEmpty
  columnCell CAPendingTasks  = ECS.cPendingTasksCount . orEmpty
  columnCell CAInstanceCount = ECS.cRegisteredContainerInstancesCount . orEmpty

defaultClusterAttrs :: [ClusterAttr]
defaultClusterAttrs = [CAName, CAStatus, CARunningTasks, CAPendingTasks, CAInstanceCount]

printClusterSummary :: Env -> IO ()
printClusterSummary env = do
  clusters <- runResourceT . runAWS env $ sourceToList fetchClusters
  printTable defaultClusterAttrs clusters
