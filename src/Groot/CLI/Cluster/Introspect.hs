module Groot.CLI.Cluster.Introspect
     ( ClusterIntrospectOpts
     , clusterIntrospectOpts
     , runClusterIntrospect
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as BS
import           Data.String
import           Data.Yaml
import qualified Options.Applicative    as Opts

import           Groot.Cluster
import           Groot.Core
import           Groot.Types

data ClusterIntrospectOpts = ClusterIntrospectOpts ClusterRef
  deriving (Eq, Show)

clusterRefArg :: Opts.Parser ClusterRef
clusterRefArg = fromString <$> Opts.argument Opts.str (Opts.metavar "CLUSTER_NAME")

clusterIntrospectOpts :: Opts.Parser ClusterIntrospectOpts
clusterIntrospectOpts = ClusterIntrospectOpts <$> clusterRefArg

runClusterIntrospect :: ClusterIntrospectOpts -> GrootIO ()
runClusterIntrospect (ClusterIntrospectOpts clusterRef) = do
  cluster <- introspectCluster clusterRef
  yaml    <- pure . BS.unpack . encode $ cluster ^. clInstanceGroups
  liftIO . putStrLn $ yaml
