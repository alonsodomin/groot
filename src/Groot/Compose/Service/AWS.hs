{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Groot.Compose.Service.AWS
     ( serviceExists'
     , verifyActiveCluster'
     , awsServiceCompose
     ) where

import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.Morph
import           Control.Monad.Reader         hiding (filterM)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict          as Map
import           Data.Maybe
import           Data.Semigroup               ((<>))
import qualified Data.Text                    as T
import           Network.AWS
import qualified Network.AWS.ECS              as ECS
import           Network.AWS.Waiter

import           Groot.Compose.Service.Free
import           Groot.Console
import           Groot.Core
import           Groot.Data.Filter
import           Groot.Data.Text
import           Groot.Exception
import           Groot.Manifest
import           Groot.Types

-- Model mapping functions

serviceDeploymentConf :: DeploymentStrategy -> ECS.DeploymentConfiguration
serviceDeploymentConf DSBlueGreen =
    ECS.dcMinimumHealthyPercent ?~ 100
  $ ECS.dcMaximumPercent ?~ 200
  $ ECS.deploymentConfiguration
serviceDeploymentConf DSRolling =
    ECS.dcMinimumHealthyPercent ?~ 50
  $ ECS.dcMaximumPercent ?~ 200
  $ ECS.deploymentConfiguration
serviceDeploymentConf DSTearDown =
    ECS.dcMinimumHealthyPercent ?~ 0
  $ ECS.dcMaximumPercent ?~ 100
  $ ECS.deploymentConfiguration

createTaskDefinitionReq :: NamedServiceDeployment -> ECS.RegisterTaskDefinition
createTaskDefinitionReq (serviceName, deployment) =
    ECS.rtdNetworkMode .~ (deployment ^. sdNetworkMode)
  $ ECS.rtdTaskRoleARN .~ (deployment ^. sdTaskRole)
  $ ECS.rtdContainerDefinitions .~ (containerDef <$> deployment ^. sdContainers)
  $ ECS.registerTaskDefinition serviceName
  where containerEnv env =
          Map.foldrWithKey (\k v acc -> (ECS.kvpName ?~ k $ ECS.kvpValue ?~ v $ ECS.keyValuePair):acc) [] env

        containerPort port =
            ECS.pmProtocol .~ (port ^. pmProtocol)
          $ ECS.pmHostPort .~ (port ^. pmHostPort)
          $ ECS.pmContainerPort ?~ (port ^. pmContainerPort)
          $ ECS.portMapping

        containerDef c =
            ECS.cdImage ?~ (c ^. cImage)
          $ ECS.cdEnvironment .~ (containerEnv $ c ^. cEnvironment)
          $ ECS.cdPortMappings .~ (containerPort <$> c ^. cPortMappings)
          $ ECS.cdMemory .~ (c ^? cMemory . _ContainerMemoryAssignment)
          $ ECS.cdMemoryReservation .~ (c ^? cMemory . _ContainerMemoryReservation)
          $ ECS.cdHostname .~ (c ^. cHostname)
          $ ECS.cdCpu .~ (c ^. cCpu)
          $ ECS.cdLogConfiguration .~ (c ^. cLogConfig)
          $ ECS.cdName ?~ (c ^. cName)
          $ ECS.cdPrivileged .~ (c ^. cPriviledged)
          $ ECS.cdEssential .~ (c ^. cEssential)
          $ ECS.cdWorkingDirectory .~ (c ^. cWorkDir)
          $ ECS.cdEntryPoint .~ (c ^. cEntryPoint)
          $ ECS.cdCommand .~ (c ^. cCommand)
          $ ECS.containerDefinition

createServiceReq :: ClusterRef -> NamedServiceDeployment -> TaskDefId -> ECS.CreateService
createServiceReq clusterRef (serviceName, deployment) tdId =
    ECS.cCluster ?~ (toText clusterRef)
  $ ECS.cRole .~ (deployment ^. sdServiceRole)
  $ ECS.cLoadBalancers .~ containerLoadBalancers
  $ ECS.cDeploymentConfiguration ?~ (serviceDeploymentConf $ deployment ^. sdDeploymentStrategy)
  $ ECS.cPlacementStrategy .~ (deployment ^. sdPlacementStrategy)
  $ ECS.createService serviceName (toText tdId) (deployment ^. sdDesiredCount)
  where loadBalancerConf container pm =
          let linkElb lnk =
                  ECS.lbContainerName ?~ (container ^. cName)
                $ ECS.lbContainerPort ?~ (pm ^. pmContainerPort)
                $ assignLink
                $ ECS.loadBalancer
                where assignLink = case lnk of
                        ELBNameLink     x -> ECS.lbLoadBalancerName ?~ x
                        TargetGroupLink x -> ECS.lbTargetGroupARN   ?~ x
          in linkElb <$> (pm ^. pmElbLink)

        containerLoadBalancers = catMaybes $ do
          cont <- deployment ^. sdContainers
          pm   <- cont ^. cPortMappings
          return $ loadBalancerConf cont pm

updateServiceReq :: ClusterRef -> NamedServiceDeployment -> TaskDefId -> ECS.UpdateService
updateServiceReq clusterRef (serviceName, deployment) tdId =
    ECS.usCluster ?~ (toText clusterRef)
  $ ECS.usTaskDefinition ?~ (toText tdId)
  $ ECS.usDesiredCount ?~ (deployment ^. sdDesiredCount)
  $ ECS.usDeploymentConfiguration ?~ (serviceDeploymentConf $ deployment ^. sdDeploymentStrategy)
  $ ECS.updateService serviceName

deleteServiceReq :: ClusterRef -> NamedServiceDeployment -> ECS.DeleteService
deleteServiceReq clusterRef (serviceName, _) =
    ECS.dsCluster ?~ (toText clusterRef)
  $ ECS.deleteService serviceName

-- Implementation helper functions

deploymentComplete :: TaskDefId -> Getter ECS.ContainerService Bool
deploymentComplete tdi = to $ isComplete . taskDeployment
  where taskDeployment :: ECS.ContainerService -> Maybe ECS.Deployment
        taskDeployment service = listToMaybe
          $ filter (\x -> maybe False (T.isSuffixOf (toText tdi)) (x ^. ECS.dTaskDefinition))
          $ service ^. ECS.csDeployments

        isComplete :: Maybe ECS.Deployment -> Bool
        isComplete mdeployment = maybe False id $ do
          deployment <- mdeployment
          running    <- deployment ^. ECS.dRunningCount
          desired    <- deployment ^. ECS.dDesiredCount
          return (running == desired)

serviceDeployed :: TaskDefId -> Wait ECS.DescribeServices
serviceDeployed taskId = Wait
  { _waitName = "serviceDeployed"
  , _waitAttempts = 100
  , _waitDelay = 6
  , _waitAcceptors =
    [ matchAny
        "MISSING"
        AcceptFailure
        (folding (concatOf ECS.dssrsFailures) . ECS.fReason . _Just . to toTextCI)
    , matchAny
        True
        AcceptSuccess
        (folding (concatOf ECS.dssrsServices) . (deploymentComplete taskId))
    ]
  }

waitForServiceDeployed :: (MonadConsole m, MonadResource m, MonadBaseControl IO m)
                       => ClusterRef
                       -> (ECS.ContainerService, Wait ECS.DescribeServices)
                       -> GrootM m ()
waitForServiceDeployed clusterRef (service, waiter) = do
  env         <- ask
  serviceName <- pure $ maybe "<unknown>" id $ service ^. ECS.csServiceName
  putInfo $ "Waiting for service "
    <> styled yellowStyle serviceName
    <> " to complete deployment..."
  deployStatus <- runAWS env $ await waiter describeIt
  case deployStatus of
    AcceptSuccess -> putSuccess $ "Service "
                     <> styled yellowStyle serviceName
                     <> " successfully deployed."
    _             -> do
      errMsg <- pure "Service did not stabilize."
      throwM $ failedServiceDeployment (ContainerServiceRef serviceName) clusterRef (Just errMsg)

  where
    describeIt :: ECS.DescribeServices
    describeIt =
        ECS.dCluster .~ (service ^. ECS.csClusterARN)
      $ ECS.dServices .~ (maybeToList $ service ^. ECS.csServiceARN)
      $ ECS.describeServices

-- Free monad operations implementation

serviceExists' :: (MonadResource m, MonadBaseControl IO m)
               => Text
               -> ClusterRef
               -> GrootM m Bool
serviceExists' serviceName clusterRef = do
  env <- ask
  hoist (runAWS env) $ awsToGrootM $ (maybe False (const True) <$> check)
  where foundServices = findService (ContainerServiceRef serviceName) (Just clusterRef)
        check         = runMaybeT $ filterM isActiveContainerService foundServices

verifyActiveCluster' :: (MonadResource m, MonadBaseControl IO m) => ClusterRef -> GrootM m ()
verifyActiveCluster' clusterRef = do
  env <- ask
  runAWS env $ do
    cluster <- getCluster clusterRef
    case (maybeMatches isActiveCluster cluster) of
      Nothing -> throwM $ invalidClusterStatus clusterRef CSInactive (Just CSActive)
      Just  _ -> return ()

registerTask' :: (MonadConsole m, MonadResource m, MonadBaseControl IO m)
              => NamedServiceDeployment
              -> GrootM m TaskDefId
registerTask' service@(serviceName, _) = do
  putInfo $ "Registering task definition for service " <> styled yellowStyle serviceName
  env   <- ask
  res   <- lift $ runAWS env . send $ createTaskDefinitionReq service
  mtask <- pure $ res ^. ECS.rtdrsTaskDefinition >>= taskDefId
  case mtask of
    Nothing   -> throwM $ failedToRegisterTaskDef (TaskDefRef serviceName)
    Just task -> do
      putSuccess $ "Task for service"
        <+> styled yellowStyle serviceName
        <+> "has been registed as"
        <+> styled yellowStyle (toText task)
      return task

modifyService' :: (MonadConsole m, MonadResource m, MonadBaseControl IO m)
               => (NamedServiceDeployment -> ClusterRef -> TaskDefId -> GrootM m (Maybe ECS.ContainerService))
               -> NamedServiceDeployment
               -> ClusterRef
               -> TaskDefId
               -> GrootM m (ECS.ContainerService, Wait ECS.DescribeServices)
modifyService' awsAction service@(serviceName, _) clusterRef taskId = do
  mservice <- awsAction service clusterRef taskId
  case mservice of
    Nothing  -> throwM $ failedServiceDeployment (ContainerServiceRef serviceName) clusterRef Nothing
    Just srv -> return (srv, serviceDeployed taskId)

createService' :: (MonadConsole m, MonadResource m, MonadBaseControl IO m)
               => NamedServiceDeployment
               -> ClusterRef
               -> TaskDefId
               -> GrootM m (ECS.ContainerService, Wait ECS.DescribeServices)
createService' = modifyService' $ \service@(serviceName, _) clusterRef taskId -> do
  putInfo $ "Creating service" <+> styled yellowStyle serviceName <+> "with task" <+> (styled yellowStyle $ toText taskId)
  env       <- ask
  createReq <- pure $ createServiceReq clusterRef service taskId
  runAWS env $ fmap (\x -> x ^. ECS.csrsService) . send $ createReq

updateService' :: (MonadConsole m, MonadResource m, MonadBaseControl IO m)
               => NamedServiceDeployment
               -> ClusterRef
               -> TaskDefId
               -> GrootM m (ECS.ContainerService, Wait ECS.DescribeServices)
updateService' = modifyService' $ \service@(serviceName, _) clusterRef taskId -> do
  putInfo $ "Updating service" <+> styled yellowStyle serviceName <+> "with task" <+> (styled yellowStyle $ toText taskId)
  env       <- ask
  updateReq <- pure $ updateServiceReq clusterRef service taskId
  runAWS env $ fmap (\x -> x ^. ECS.usrsService) . send $ updateReq

removeService' :: (MonadConsole m, MonadResource m, MonadBaseControl IO m)
               => NamedServiceDeployment
               -> ClusterRef
               -> GrootM m ()
removeService' service@(serviceName, _) clusterRef = do
  putInfo $ "Deleting service" <+> styled yellowStyle serviceName <> "."
  env       <- ask

  let csRef = ContainerServiceRef serviceName
  current   <- runAWS env $ runMaybeT $ findService csRef (Just clusterRef)

  case current of
    Nothing -> throwM $ serviceNotFound csRef (Just clusterRef)
    Just  c -> do
      updateReq <- pure $ ECS.usDesiredCount ?~ 0 $ ECS.usCluster ?~ (toText clusterRef) $ ECS.updateService serviceName
      runAWS env $ send updateReq
      deleteReq <- pure $ deleteServiceReq clusterRef service
      runAWS env $ send deleteReq
      return ()

awsServiceCompose :: (MonadConsole m, MonadResource m, MonadBaseControl IO m) => ServiceComposeM a -> GrootM m a
awsServiceCompose = foldFree $ \case
  RegisterTask service next ->
    next <$> registerTask' service
  ServiceExists name clusterRef next ->
    next <$> serviceExists' name clusterRef
  CreateService service cluster taskId next ->
    (const next) <$> (createService' service cluster taskId >>= waitForServiceDeployed cluster)
  UpdateService service cluster taskId next ->
    (const next) <$> (updateService' service cluster taskId >>= waitForServiceDeployed cluster)
  RemoveService service cluster next ->
    (const next) <$> removeService' service cluster
  VerifyActiveCluster cluster next ->
    (const next) <$> verifyActiveCluster' cluster
