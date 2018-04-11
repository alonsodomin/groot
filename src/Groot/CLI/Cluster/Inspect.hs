module Groot.CLI.Cluster.Inspect
     ( ClusterInspectOpts
     , clusterInspectOpts
     , runClusterInspect
     ) where

import           Options.Applicative
import Data.String

import Groot.Core
import Groot.Types

data ClusterInspectOpts = ClusterInspectOpts ClusterRef
  deriving (Eq, Show)

clusterRefArg :: Parser ClusterRef
clusterRefArg = fromString <$> argument str (metavar "CLUSTER_NAME")

clusterInspectOpts :: Parser ClusterInspectOpts
clusterInspectOpts = ClusterInspectOpts <$> clusterRefArg

runClusterInspect :: ClusterInspectOpts -> GrootM IO ()
runClusterInspect = undefined