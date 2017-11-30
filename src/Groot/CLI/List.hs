module Groot.CLI.List
     ( ListSubCmd
     , listCmds
     , runListCmd
     ) where

import           Data.Maybe                       (maybeToList)
import           Data.Semigroup                   ((<>))
import           Network.AWS
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.CLI.List.Cluster
import           Groot.CLI.List.ContainerInstance
import           Groot.CLI.List.ContainerService
import           Groot.CLI.List.Task
import           Groot.CLI.List.TaskDef
import           Groot.Types

data ListSubCmd =
    ListClustersCmd (Maybe ClusterRef)
  | ListInstancesCmd (Maybe ClusterRef)
  | ListTasksCmd (Maybe ClusterRef)
  | ListTaskDefsCmd Bool (Maybe TaskFamily)
  | ListServicesCmd (Maybe ClusterRef)
  deriving (Eq, Show)

listClustersCmd :: Parser ListSubCmd
listClustersCmd = ListClustersCmd <$> optional clusterOpt

listInstancesCmd :: Parser ListSubCmd
listInstancesCmd = ListInstancesCmd <$> optional clusterOpt

listTasksCmd :: Parser ListSubCmd
listTasksCmd = ListTasksCmd <$> optional clusterOpt

listTaskDefsCmd :: Parser ListSubCmd
listTaskDefsCmd = ListTaskDefsCmd
              <$> switch
                ( long "inactive"
                <> short 'i'
                <> help "Show inactive task definitions" )
              <*> optional taskFamilyOpt

listServicesCmd :: Parser ListSubCmd
listServicesCmd = ListServicesCmd <$> optional clusterOpt

listCmds :: Parser ListSubCmd
listCmds = hsubparser
  ( command "clusters"  (info listClustersCmd  (progDesc "List clusters"))
 <> command "instances" (info listInstancesCmd (progDesc "List instances"))
 <> command "tasks"     (info listTasksCmd     (progDesc "List tasks"))
 <> command "taskDefs"  (info listTaskDefsCmd  (progDesc "List task definitions"))
 <> command "services"  (info listServicesCmd  (progDesc "List services"))
  )

runListCmd :: ListSubCmd -> Env -> IO ()
runListCmd (ListClustersCmd clusterId)        = printClusterSummary clusterId
runListCmd (ListInstancesCmd clusterId)       = printInstanceSummary clusterId
runListCmd (ListTasksCmd clusterId)           = printTaskSummary clusterId
runListCmd (ListServicesCmd clusterId)        = printServiceSummary clusterId
runListCmd (ListTaskDefsCmd showInactive fam) =
  let statusFilter = if showInactive then [TDFStatus TDSInactive] else []
      familyFilter = maybeToList $ TDFFamily <$> fam
  in printTaskDefsSummary $ statusFilter ++ familyFilter
