{-# LANGUAGE OverloadedStrings #-}

module Groot.CLI.Cluster.Inspect
     ( ClusterInspectOpts
     , clusterInspectOpts
     , runClusterInspect
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Maybe
import           Data.String
import           Data.Text                  (Text)
import           Network.AWS
import qualified Network.AWS.ECS            as ECS
import           Options.Applicative        (Parser)
import qualified Options.Applicative        as Opts

import           Groot.Core
import           Groot.Internal.PrettyPrint (Doc, defaultIndent, (<+>))
import qualified Groot.Internal.PrettyPrint as Doc
import           Groot.Types

data ClusterInspectOpts = ClusterInspectOpts ClusterRef
  deriving (Eq, Show)

clusterRefArg :: Parser ClusterRef
clusterRefArg = fromString <$> Opts.argument Opts.str (Opts.metavar "CLUSTER_NAME")

clusterInspectOpts :: Parser ClusterInspectOpts
clusterInspectOpts = ClusterInspectOpts <$> clusterRefArg

pprintCluster :: ECS.Cluster -> [ECS.ContainerInstance] -> Doc
pprintCluster cluster nodes = Doc.vsep [
      Doc.bold . Doc.dullblue $ maybe mempty Doc.pretty $ cluster ^. ECS.cClusterName
    , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
        Doc.field Doc.status "Status:" <$> cluster ^. ECS.cStatus
      , Doc.field' "Active Services:" <$> cluster ^. ECS.cActiveServicesCount
      , Doc.field' "Running Tasks:" <$> cluster ^. ECS.cRunningTasksCount
      , Doc.field' "Pending Tasks:" <$> cluster ^. ECS.cPendingTasksCount
      , Doc.listField ppInstance "Nodes:" nodes
    ])
  ]
  where ppInstance :: ECS.ContainerInstance -> Doc
        ppInstance inst = Doc.vsep [
              Doc.hyphen <+> (Doc.bold . Doc.dullblue $ maybe mempty Doc.pretty $ inst ^. ECS.ciEc2InstanceId)
            , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
                Doc.field Doc.status "Status:" <$> inst ^. ECS.ciStatus
              , Doc.field' "Connected:" <$> inst ^. ECS.ciAgentConnected
              , Doc.field' "Running Tasks:" <$> inst ^. ECS.ciRunningTasksCount
              , Doc.field' "Pending Tasks:" <$> inst ^. ECS.ciPendingTasksCount
              , Doc.field Doc.defaultTime "Registered At:" <$> inst ^. ECS.ciRegisteredAt
            ])
          ]

runClusterInspect :: ClusterInspectOpts -> GrootM IO ()
runClusterInspect (ClusterInspectOpts clusterRef) = do
  env              <- ask
  (cluster, nodes) <- runResourceT . runAWS env $ do
    c <- getCluster clusterRef
    n <- runConduit $ fetchInstances clusterRef =$ CL.consume
    return (c, n)
  liftIO . Doc.putDoc $ pprintCluster cluster nodes
