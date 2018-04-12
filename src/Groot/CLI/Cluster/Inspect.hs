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
import qualified Network.AWS.EC2            as EC2
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

pprintCluster :: ECS.Cluster -> [(ECS.ContainerInstance, EC2.Instance)] -> Doc
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
  where ppInstance :: (ECS.ContainerInstance, EC2.Instance) -> Doc
        ppInstance (ecsInst, ec2Inst) = Doc.vsep [
              Doc.hyphen <+> (Doc.bold . Doc.dullblue $ maybe mempty Doc.pretty $ ecsInst ^. ECS.ciEc2InstanceId)
            , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
                Doc.field Doc.status "Status:" <$> ecsInst ^. ECS.ciStatus
              , Doc.field' "Connected:" <$> ecsInst ^. ECS.ciAgentConnected
              , Doc.field' "Private IP:" <$> ec2Inst ^. EC2.insPrivateIPAddress
              , Doc.field' "Running Tasks:" <$> ecsInst ^. ECS.ciRunningTasksCount
              , Doc.field' "Pending Tasks:" <$> ecsInst ^. ECS.ciPendingTasksCount
              , Doc.field Doc.defaultTime "Registered At:" <$> ecsInst ^. ECS.ciRegisteredAt
            ])
          ]

runClusterInspect :: ClusterInspectOpts -> GrootM IO ()
runClusterInspect (ClusterInspectOpts clusterRef) = do
  env              <- ask

  (cluster, nodes) <- runResourceT . runAWS env $ do
    clus        <- getCluster clusterRef
    fromCluster <- runConduit $ fetchInstances clusterRef =$ CL.consume
    ids         <- pure $ fmap EC2InstanceId $ catMaybes $ (view ECS.ciEc2InstanceId) <$> fromCluster
    fromEc2     <- runConduit $ findEc2Instances ids =$ CL.consume
    return (clus, zip fromCluster fromEc2)

  liftIO . Doc.putDoc $ pprintCluster cluster nodes
