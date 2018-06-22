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
import qualified Data.HashMap.Lazy          as Map
import           Data.Maybe
import           Data.Monoid
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

data InspectFlag =
  ShowInstanceAttrs
  deriving (Eq, Show)

showInstanceAttrsOpt :: Parser InspectFlag
showInstanceAttrsOpt = Opts.flag' ShowInstanceAttrs
                     ( Opts.long "show-instance-attrs"
                    <> Opts.help "displays instance attributes" )

inspectFlagOpts :: Parser [InspectFlag]
inspectFlagOpts = Opts.many showInstanceAttrsOpt

data ClusterInspectOpts = ClusterInspectOpts ClusterRef [InspectFlag]
  deriving (Eq, Show)

clusterInspectOpts :: Parser ClusterInspectOpts
clusterInspectOpts = ClusterInspectOpts
                  <$> clusterRefArg
                  <*> inspectFlagOpts

clusterRefArg :: Parser ClusterRef
clusterRefArg = fromString <$> Opts.argument Opts.str (Opts.metavar "CLUSTER_NAME")

pprintCluster :: ECS.Cluster -> [(ECS.ContainerInstance, Maybe EC2.Instance)] -> [InspectFlag] -> Doc
pprintCluster cluster nodes flags = Doc.vsep [
      Doc.bold . Doc.dullblue $ maybe mempty Doc.pretty $ cluster ^. ECS.cClusterName
    , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
        Doc.field Doc.status "Status:" <$> cluster ^. ECS.cStatus
      , Doc.field' "Active Services:" <$> cluster ^. ECS.cActiveServicesCount
      , Doc.field' "Running Tasks:" <$> cluster ^. ECS.cRunningTasksCount
      , Doc.field' "Pending Tasks:" <$> cluster ^. ECS.cPendingTasksCount
      , Doc.listField ppInstance "Nodes:" nodes
    ])
  ]
  where showInstanceAttrs = elem ShowInstanceAttrs flags

        ppInstance :: (ECS.ContainerInstance, Maybe EC2.Instance) -> Doc
        ppInstance (ecsInst, ec2Inst) = Doc.vsep [
              Doc.bold . Doc.dullblue $ maybe mempty Doc.pretty $ ecsInst ^. ECS.ciEc2InstanceId
            , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
                Doc.field Doc.status "Status:" <$> ecsInst ^. ECS.ciStatus
              , Doc.field' "Connected:" <$> ecsInst ^. ECS.ciAgentConnected
              , Doc.field' "Private IP:" <$> (ec2Inst >>= view EC2.insPrivateIPAddress)
              , Doc.field' "Running Tasks:" <$> ecsInst ^. ECS.ciRunningTasksCount
              , Doc.field' "Pending Tasks:" <$> ecsInst ^. ECS.ciPendingTasksCount
              , Doc.field Doc.defaultTime "Registered At:" <$> ecsInst ^. ECS.ciRegisteredAt
              , if showInstanceAttrs then Doc.listField ppInstanceAttr "Attributes:" $ ecsInst ^. ECS.ciAttributes
                else Nothing
            ])
          ]

        ppInstanceAttr :: ECS.Attribute -> Doc
        ppInstanceAttr attr = Doc.hsep $ catMaybes [
              Just (Doc.bold . Doc.dullblue . Doc.pretty $ attr ^. ECS.aName)
            , (\x -> Doc.pretty ("=" :: Text) <+> Doc.pretty x) <$> attr ^. ECS.aValue
          ]

runClusterInspect :: ClusterInspectOpts -> GrootIO ()
runClusterInspect (ClusterInspectOpts clusterRef flags) = GrootT $ do
  env              <- ask

  (cluster, nodes) <- runResourceT . runAWS env $ do
    clus        <- getCluster clusterRef
    fromCluster <- runConduit $ fetchInstances clusterRef .| CL.consume
    ids         <- pure $ fmap EC2InstanceId $ catMaybes $ (view ECS.ciEc2InstanceId) <$> fromCluster
    fromEc2     <- runConduit $ findEc2Instances ids .| CL.consume
    return (clus, pairInstances fromCluster fromEc2)

  liftIO . Doc.putDoc $ pprintCluster cluster nodes flags

  where pairInstances :: [ECS.ContainerInstance] -> [EC2.Instance] -> [(ECS.ContainerInstance, Maybe EC2.Instance)]
        pairInstances ecsInsts ec2Insts =
          let ec2InstanceMap = Map.fromList $ (\x -> (x ^. EC2.insInstanceId, x)) <$> ec2Insts
          in fmap (\x -> (x, (flip Map.lookup) ec2InstanceMap =<< (x ^. ECS.ciEc2InstanceId))) ecsInsts
