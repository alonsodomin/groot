module Groot.CLI.Cluster.Introspect
     ( ClusterIntrospectOpts
     , clusterIntrospectOpts
     , runClusterIntrospect
     ) where

import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Data.String
import qualified Options.Applicative    as Opts

import           Groot.Core
import           Groot.Types

data ClusterIntrospectOpts = ClusterIntrospectOpts ClusterRef
  deriving (Eq, Show)

clusterRefArg :: Opts.Parser ClusterRef
clusterRefArg = fromString <$> Opts.argument Opts.str (Opts.metavar "CLUSTER_NAME")

clusterIntrospectOpts :: Opts.Parser ClusterIntrospectOpts
clusterIntrospectOpts = ClusterIntrospectOpts <$> clusterRefArg

runClusterIntrospect :: ClusterIntrospectOpts -> GrootIO ()
runClusterIntrospect (ClusterIntrospectOpts clusterRef) =
  runGrootResource . runConduit $ transPipe awsResource (findAutoScalingGroups clusterRef) .| CL.mapM_ (\x -> liftIO . print $ x)
