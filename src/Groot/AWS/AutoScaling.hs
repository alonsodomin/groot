module Groot.AWS.AutoScaling
     ( findAutoScalingGroups
     ) where

import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Text               (Text)
import           Network.AWS
import qualified Network.AWS.AutoScaling as AS
import qualified Network.AWS.ECS         as ECS

import           Groot.AWS.Instance
import           Groot.Types

findAutoScalingGroups :: MonadAWS m => ClusterRef -> ConduitT () AS.AutoScalingGroup m ()
findAutoScalingGroups clusterRef = clusterInstanceIds .| autoScalingGroupNames .| autoScalingGroups
  where clusterInstanceIds :: MonadAWS m => ConduitT () [Text] m ()
        clusterInstanceIds = fetchInstances clusterRef .| CL.mapMaybe (view ECS.ciEc2InstanceId) .| CL.chunksOf 50

        autoScalingGroupNames :: MonadAWS m => ConduitT [Text] [Text] m ()
        autoScalingGroupNames = awaitForever (\ids -> toProducer . paginate $ AS.dasiInstanceIds .~ ids $ AS.describeAutoScalingInstances)
          .| CL.concatMap (view AS.dasirsAutoScalingInstances)
          .| CL.map (view AS.asidAutoScalingGroupName)
          .| CL.chunksOf 10

        autoScalingGroups :: MonadAWS m => ConduitT [Text] AS.AutoScalingGroup m ()
        autoScalingGroups = awaitForever (\names -> toProducer . paginate $ AS.dasgAutoScalingGroupNames .~ names $ AS.describeAutoScalingGroups)
          .| CL.concatMap (view AS.dasgrsAutoScalingGroups)
