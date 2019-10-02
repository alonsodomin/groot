{-# LANGUAGE OverloadedStrings #-}

module Groot.CLI.Cluster.Update
     ( ClusterUpdateOpts
     , clusterUpdateOpts
     , runClusterUpdate
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Conduit               hiding (await)
import           Data.Conduit.Async         (runCConduit, (=$=&))
import qualified Data.Conduit.List          as CL
import           Data.String
import           Network.AWS
import qualified Network.AWS.ECS            as ECS
import           Network.AWS.Waiter
import           Options.Applicative        (Parser)
import qualified Options.Applicative        as Opts

import           Groot.Console
import           Groot.Core
import           Groot.Internal.Data.Filter
import           Groot.Internal.Data.Text
import           Groot.Types

data ClusterUpdateOpts = ClusterUpdateOpts ClusterRef
  deriving (Eq, Show)

clusterUpdateOpts :: Parser ClusterUpdateOpts
clusterUpdateOpts = ClusterUpdateOpts
                <$> clusterRefArg

clusterRefArg :: Parser ClusterRef
clusterRefArg = fromString <$> Opts.argument Opts.str (Opts.metavar "CLUSTER_NAME")

instanceAgentUpdated :: Wait ECS.DescribeContainerInstances
instanceAgentUpdated = Wait
  { _waitName = "instanceAgentUpdated"
  , _waitAttempts = 20
  , _waitDelay = 5
  , _waitAcceptors =
    [ matchAny
        "MISSING"
        AcceptFailure
        (folding (concatOf ECS.dcisrsFailures) . ECS.fReason . _Just . to toTextCI)
    , matchAll
        ECS.AUSUpdated
        AcceptSuccess
        (folding (concatOf ECS.dcisrsContainerInstances) . ECS.ciAgentUpdateStatus . _Just)
    ]
  }

updateAgentAndWait :: ClusterRef -> ContainerInstanceRef -> GrootIOResource ()
updateAgentAndWait clusterRef instRef =
  let updateAction = awsResource_ $ do
        liftIO . putInfo $ "Updating ECS agent on cluster instance" <+> (styled yellowStyle $ toText instRef)
        updateAgent clusterRef instRef
        let describeReq = ECS.dciCluster ?~ (toText clusterRef)
                        $ ECS.dciContainerInstances .~ [(toText instRef)]
                        $ ECS.describeContainerInstances
        await instanceAgentUpdated describeReq
        liftIO . putSuccess $ "ECS agent for instance"
          <+> (styled yellowStyle $ toText instRef)
          <+> "successfully updated."

      alreadyUpToDate = liftIO . putSuccess $ "ECS agent for instance"
                    <+> (styled yellowStyle $ toText instRef)
                    <+> "is already up to date"
  in catching ECS._NoUpdateAvailableException updateAction (\_ -> alreadyUpToDate)

runClusterUpdate :: ClusterUpdateOpts -> GrootIO ()
runClusterUpdate (ClusterUpdateOpts clusterRef) =
  useResource . runCConduit $ instanceStream =$=& CL.mapM_ (updateAgentAndWait clusterRef)
  where instanceStream :: ConduitT () ContainerInstanceRef GrootIOResource ()
        instanceStream = transPipe awsResource $ fetchInstances clusterRef
          .| filterC canUpdateContainerAgent
          .| CL.mapMaybe instanceRef
