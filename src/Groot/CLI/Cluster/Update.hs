{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.Cluster.Update where

import           Options.Applicative        (Parser)
import qualified Options.Applicative        as Opts
import           Data.String
import           Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.AWS

import           Groot.Core
import Groot.Console
import Groot.Data.Filter
import           Groot.Types
import           Groot.Data.Text

data ClusterUpdateOpts = ClusterUpdateOpts ClusterRef
  deriving (Eq, Show)

clusterUpdateOpts :: Parser ClusterUpdateOpts
clusterUpdateOpts = ClusterUpdateOpts
                <$> clusterRefArg

clusterRefArg :: Parser ClusterRef
clusterRefArg = fromString <$> Opts.argument Opts.str (Opts.metavar "CLUSTER_NAME")

runClusterUpdate :: ClusterUpdateOpts -> GrootIO ()
runClusterUpdate (ClusterUpdateOpts clusterRef) = runGrootResource . awsResource . runConduit $ instanceStream
  where instanceStream :: ConduitT () Void AWS ()
        instanceStream = fetchInstances clusterRef
          .| filterC canUpdateContainerAgent
          .| CL.mapMaybe instanceRef
          .| CL.mapM_ doAgentUpdate

        doAgentUpdate :: ContainerInstanceRef -> AWS ()
        doAgentUpdate instRef = do
          liftIO . putInfo $ "Updating ECS agent on cluster instance" <+> (styled yellowStyle $ toText instRef)
          updateAgent clusterRef instRef