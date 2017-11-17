module Groot.App.Cluster
     ( ClusterOptions
     , grootClusterCli
     , runGrootCluster
     ) where

import Network.AWS
import Options.Applicative

import Groot.App.Cluster.Events

data ClusterCmd =
  ClusterEventsCmd ClusterEventOptions
  deriving (Eq, Show)

data ClusterOptions = ClusterOptions ClusterCmd
  deriving (Eq, Show)

-- CLI

clusterEventsCmd :: Parser ClusterCmd
clusterEventsCmd = ClusterEventsCmd <$> clusterEventsCli

clusterCmds :: Parser ClusterCmd
clusterCmds = hsubparser
  ( command "events" (info clusterEventsCmd (progDesc "Display events of the given clusters"))
  )

grootClusterCli :: Parser ClusterOptions
grootClusterCli = ClusterOptions <$> clusterCmds

-- run function

runGrootCluster :: ClusterOptions -> Env -> IO ()
runGrootCluster (ClusterOptions (ClusterEventsCmd eventsOpts)) = runClusterEvents eventsOpts