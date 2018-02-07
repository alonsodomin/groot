module Groot.Compose.Service
     ( module Groot.Compose.Service.Model
     , deployService
     , deployServices
     , awsServiceCompose
     , dryRunServiceCompose
     ) where

import           Groot.Compose.Service.AWS (awsServiceCompose)
import           Groot.Compose.Service.Free
import           Groot.Compose.Service.Model
import           Groot.Compose.Service.DryRun

runServiceDeploy :: (MonadResource m) => ServiceCompose -> [Text] -> ClusterRef -> Bool -> GrootM m ()
runServiceDeploy composeDef serviceNames clusterRef dryRun = do
  
  where selectServices :: MonadThrow m => [Text] -> HashMap Text ServiceDeployment -> m [(Text, ServiceDeployment)]
        selectServices = undefined