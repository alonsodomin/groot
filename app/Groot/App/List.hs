{-# LANGUAGE OverloadedStrings #-}

module Groot.App.List
     ( ListOptions(..)
     , grootListCli
     , runGrootList
     ) where

import Data.Maybe (maybeToList)
import Data.Semigroup ((<>))
import Network.AWS
import Options.Applicative

import Groot.App.Cli.Parsers
     ( clusterOpt
     , taskFamilyOpt
     )
import Groot.App.List.Cluster
import Groot.App.List.Instance
import Groot.App.List.Task
import Groot.App.List.TaskDef
import Groot.App.List.Service
import Groot.Data

data ListCmd =
    ClustersCmd
  | InstancesCmd (Maybe ClusterRef)
  | TasksCmd (Maybe ClusterRef)
  | TaskDefsCmd Bool (Maybe TaskFamily)
  | ServicesCmd (Maybe ClusterRef)
  deriving (Eq, Show)

data ListOptions = ListOptions ListCmd deriving (Eq, Show)

-- CLI

clustersCli :: Parser ListCmd
clustersCli = pure ClustersCmd

instancesCli :: Parser ListCmd
instancesCli = InstancesCmd <$> optional clusterOpt

tasksCli :: Parser ListCmd
tasksCli = TasksCmd <$> optional clusterOpt

taskDefsCli :: Parser ListCmd
taskDefsCli = TaskDefsCmd
          <$> switch
            ( long "inactive"
           <> short 'i'
           <> help "Show inactive task definitions" )
          <*> optional taskFamilyOpt

servicesCli :: Parser ListCmd
servicesCli = ServicesCmd <$> optional clusterOpt

listCmds :: Parser ListCmd
listCmds = hsubparser
  ( command "clusters"  (info clustersCli  (progDesc "List clusters"))
 <> command "instances" (info instancesCli (progDesc "List instances"))
 <> command "tasks"     (info tasksCli     (progDesc "List tasks"))
 <> command "taskDefs"  (info taskDefsCli  (progDesc "List task definitions"))
 <> command "services"  (info servicesCli  (progDesc "List services"))
  )

grootListCli :: Parser ListOptions
grootListCli = ListOptions <$> listCmds

-- Run function

runGrootList :: ListOptions -> Env -> IO ()
runGrootList (ListOptions  ClustersCmd)                   = printClusterSummary
runGrootList (ListOptions (InstancesCmd clusterId))       = printInstanceSummary clusterId
runGrootList (ListOptions (TasksCmd clusterId))           = printTaskSummary clusterId
runGrootList (ListOptions (ServicesCmd clusterId))        = printServiceSummary clusterId
runGrootList (ListOptions (TaskDefsCmd showInactive fam)) =
  let statusFilter = if showInactive then [StatusFilter TaskInactive] else []
      familyFilter = maybeToList $ FamilyFilter <$> fam
  in printTaskDefsSummary $ statusFilter ++ familyFilter
