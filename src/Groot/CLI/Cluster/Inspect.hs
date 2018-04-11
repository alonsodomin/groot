module Groot.CLI.Cluster.Inspect
     ( ClusterInspectOpts
     , clusterInspectOpts
     , runClusterInspect
     ) where

import           Data.String
import           Network.AWS
import qualified Network.AWS.ECS     as ECS
import           Options.Applicative

import           Groot.Core
import           Groot.Types

data ClusterInspectOpts = ClusterInspectOpts ClusterRef
  deriving (Eq, Show)

clusterRefArg :: Parser ClusterRef
clusterRefArg = fromString <$> argument str (metavar "CLUSTER_NAME")

clusterInspectOpts :: Parser ClusterInspectOpts
clusterInspectOpts = ClusterInspectOpts <$> clusterRefArg

--pprintCluster :: ECS.Cluster ->

runClusterInspect :: ClusterInspectOpts -> GrootM IO ()
runClusterInspect = undefined
