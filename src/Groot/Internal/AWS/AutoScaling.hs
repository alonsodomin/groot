module Groot.Internal.AWS.AutoScaling
     ( findAutoScalingGroups
     ) where

import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.Text                   (Text)
import           Network.AWS
import qualified Network.AWS.AutoScaling     as AS
import qualified Network.AWS.ECS             as ECS

import           Groot.Internal.AWS.Instance
import           Groot.Types

findAutoScalingInstances :: MonadAWS m => ConduitT ClusterRef AS.AutoScalingInstanceDetails m ()
findAutoScalingInstances = awaitForever (\cref -> toProducer $ clusterInstanceIds cref) .| autoScalingInstances
  where clusterInstanceIds :: MonadAWS m => ClusterRef -> ConduitT () Text m ()
        clusterInstanceIds clusterRef = fetchInstances clusterRef
          .| CL.mapMaybe (view ECS.ciEc2InstanceId)

        autoScalingInstances :: MonadAWS m => ConduitT Text AS.AutoScalingInstanceDetails m ()
        autoScalingInstances = CL.chunksOf 50
          .| awaitForever (\ids -> toProducer . paginate $ AS.dasiInstanceIds .~ ids $ AS.describeAutoScalingInstances)
          .| CL.concatMap (view AS.dasirsAutoScalingInstances)

findAutoScalingGroups :: MonadAWS m => ConduitT ClusterRef AS.AutoScalingGroup m ()
findAutoScalingGroups = findAutoScalingInstances .| autoScalingGroups
  where autoScalingGroups :: MonadAWS m => ConduitT AS.AutoScalingInstanceDetails AS.AutoScalingGroup m ()
        autoScalingGroups = CL.map (view AS.asidAutoScalingGroupName)
          .| CL.chunksOf 10
          .| awaitForever (\names -> toProducer . paginate $ AS.dasgAutoScalingGroupNames .~ names $ AS.describeAutoScalingGroups)
          .| CL.concatMap (view AS.dasgrsAutoScalingGroups)
