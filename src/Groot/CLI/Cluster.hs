module Groot.CLI.Cluster
     ( ClusterSubCmd
     , clusterCmds
     , runClusterCmd
     ) where

import           Options.Applicative

import           Groot.CLI.Cluster.Events
import           Groot.Core

data ClusterSubCmd =
  ClusterEventsCmd ClusterEventOptions
  deriving (Eq, Show)

-- CLI

clusterEventsCmd :: Parser ClusterSubCmd
clusterEventsCmd = ClusterEventsCmd <$> clusterEventsOpt

clusterCmds :: Parser ClusterSubCmd
clusterCmds = hsubparser
  ( command "events" (info clusterEventsCmd (progDesc "Display events of the given clusters"))
  )

-- run function

runClusterCmd :: ClusterSubCmd -> GrootM IO ()
runClusterCmd (ClusterEventsCmd eventsOpts) = runClusterEvents eventsOpts
