module Groot.CLI.List
     ( ListSubCmd
     , listCmds
     , runListCmd
     ) where

import           Data.Semigroup                   ((<>))
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.CLI.List.Cluster
import           Groot.CLI.List.ContainerInstance
import           Groot.CLI.List.ContainerService
import           Groot.CLI.List.Task
import           Groot.CLI.List.TaskDef
import           Groot.CLI.List.Volume
import           Groot.Core
import           Groot.Types

data ListSubCmd =
    ListClustersCmd (Maybe ClusterRef)
  | ListInstancesCmd (Maybe ClusterRef)
  | ListTasksCmd ListTaskOpts
  | ListTaskDefsCmd ListTaskDefsOpts
  | ListServicesCmd (Maybe ClusterRef)
  | ListVolumesCmd ListVolumeOpts
  deriving (Eq, Show)

listClustersCmd :: Parser ListSubCmd
listClustersCmd = ListClustersCmd <$> optional clusterOpt

listInstancesCmd :: Parser ListSubCmd
listInstancesCmd = ListInstancesCmd <$> optional clusterOpt

listTasksCmd :: Parser ListSubCmd
listTasksCmd = ListTasksCmd <$> listTaskOpts

listTaskDefsCmd :: Parser ListSubCmd
listTaskDefsCmd = ListTaskDefsCmd <$> listTaskDefsOpts

listServicesCmd :: Parser ListSubCmd
listServicesCmd = ListServicesCmd <$> optional clusterOpt

listVolumesCmd :: Parser ListSubCmd
listVolumesCmd = ListVolumesCmd <$> listVolumeOpts

listCmds :: Parser ListSubCmd
listCmds = hsubparser
  ( command "clusters"  (info listClustersCmd  (progDesc "List clusters"))
 <> command "instances" (info listInstancesCmd (progDesc "List instances"))
 <> command "tasks"     (info listTasksCmd     (progDesc "List tasks"))
 <> command "taskDefs"  (info listTaskDefsCmd  (progDesc "List task definitions"))
 <> command "services"  (info listServicesCmd  (progDesc "List services"))
 <> command "volumes"   (info listVolumesCmd   (progDesc "List volumes"))
  )

runListCmd :: ListSubCmd -> GrootIO ()
runListCmd (ListClustersCmd clusterId)  = printClusterSummary  clusterId
runListCmd (ListInstancesCmd clusterId) = printInstanceSummary clusterId
runListCmd (ListTasksCmd opts)          = printTaskSummary     opts
runListCmd (ListServicesCmd clusterId)  = printServiceSummary  clusterId
runListCmd (ListTaskDefsCmd opts)       = printTaskDefsSummary opts
runListCmd (ListVolumesCmd opts)        = printVolumesSummary  opts
