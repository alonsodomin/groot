module Groot.CLI.Cluster
     ( ClusterSubCmd
     , clusterCmds
     , runClusterCmd
     ) where

import           Data.Monoid
import           Options.Applicative

import           Groot.CLI.Cluster.Events
import           Groot.CLI.Cluster.Inspect
import           Groot.Core

data ClusterSubCmd =
    ClusterEventsCmd  ClusterEventOptions
  | ClusterInspectCmd ClusterInspectOpts
  deriving (Eq, Show)

-- CLI

clusterEventsCmd :: Parser ClusterSubCmd
clusterEventsCmd = ClusterEventsCmd <$> clusterEventsOpt

clusterInspectCmd :: Parser ClusterSubCmd
clusterInspectCmd = ClusterInspectCmd <$> clusterInspectOpts

clusterCmds :: Parser ClusterSubCmd
clusterCmds = hsubparser
  ( command "events"  (info clusterEventsCmd  (progDesc "Display events of the given clusters"))
 <> command "inspect" (info clusterInspectCmd (progDesc "Inspect details of a given cluster"))
  )

-- run function

runClusterCmd :: ClusterSubCmd -> GrootIO ()
runClusterCmd (ClusterEventsCmd  eventsOpts)  = runClusterEvents  eventsOpts
runClusterCmd (ClusterInspectCmd inspectOpts) = runClusterInspect inspectOpts
