module Groot.Internal.AWS.AutoScaling
     ( findLaunchConfiguration
     , fetchAutoScalingGroups
     ) where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.Maybe
import           Data.Text                   (Text)
import           Network.AWS
import qualified Network.AWS.AutoScaling     as AS
import qualified Network.AWS.ECS             as ECS

import           Groot.Internal.AWS.Instance
import           Groot.Types

findLaunchConfiguration :: MonadAWS m => Text -> MaybeT m AS.LaunchConfiguration
findLaunchConfiguration name = MaybeT $ do
  res <- send $ AS.dlcLaunchConfigurationNames .~ [name] $ AS.describeLaunchConfigurations
  return . listToMaybe $ res ^. AS.dlcrsLaunchConfigurations

fetchAutoScalingInstances :: MonadAWS m => ConduitT ClusterRef AS.AutoScalingInstanceDetails m ()
fetchAutoScalingInstances = awaitForever (\cref -> toProducer $ clusterInstanceIds cref) .| autoScalingInstances
  where clusterInstanceIds :: MonadAWS m => ClusterRef -> ConduitT () Text m ()
        clusterInstanceIds clusterRef = fetchInstances clusterRef
          .| CL.mapMaybe (view ECS.ciEc2InstanceId)

        autoScalingInstances :: MonadAWS m => ConduitT Text AS.AutoScalingInstanceDetails m ()
        autoScalingInstances = CL.chunksOf 50
          .| awaitForever (\ids -> toProducer . paginate $ AS.dasiInstanceIds .~ ids $ AS.describeAutoScalingInstances)
          .| CL.concatMap (view AS.dasirsAutoScalingInstances)

fetchAutoScalingGroups :: MonadAWS m => ConduitT ClusterRef (AS.AutoScalingGroup, AS.LaunchConfiguration) m ()
fetchAutoScalingGroups = fetchAutoScalingInstances .| autoScalingGroups .| launchConfigs
  where autoScalingGroups :: MonadAWS m => ConduitT AS.AutoScalingInstanceDetails AS.AutoScalingGroup m ()
        autoScalingGroups = CL.map (view AS.asidAutoScalingGroupName)
          .| CL.chunksOf 10
          .| awaitForever (\names -> toProducer . paginate $ AS.dasgAutoScalingGroupNames .~ names $ AS.describeAutoScalingGroups)
          .| CL.concatMap (view AS.dasgrsAutoScalingGroups)

        launchConfigs :: MonadAWS m => ConduitT AS.AutoScalingGroup (AS.AutoScalingGroup, AS.LaunchConfiguration) m ()
        launchConfigs =
          let liftedConfigName :: MonadAWS m => AS.AutoScalingGroup -> MaybeT m Text
              liftedConfigName group = MaybeT . pure $ group ^. AS.asgLaunchConfigurationName

              launchConfigRef :: MonadAWS m => AS.AutoScalingGroup -> m (Maybe (AS.AutoScalingGroup, AS.LaunchConfiguration))
              launchConfigRef group = runMaybeT $ ((fmap ((,) group)) . findLaunchConfiguration) =<< (liftedConfigName group)
          in CL.mapMaybeM launchConfigRef
