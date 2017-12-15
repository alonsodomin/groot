{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Groot.Core.Compose
     ( GrootCompose
     , composeServices
     ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Reader           hiding (filterM)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe
import           Data.Semigroup                 ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS                as ECS

import           Groot.Core
import           Groot.Core.Console
import           Groot.Data.Filter
import           Groot.Data.Text
import           Groot.Exception
import           Groot.Types

data PortELBLink =
    ELBNameLink Text
  | TargetGroupLink Text
  deriving (Eq, Show)

data PortMapping = PortMapping
  { _pmContainerPort :: Int
  , _pmHostPort      :: Maybe Int
  , _pmProtocol      :: Maybe ECS.TransportProtocol
  , _pmElbLink       :: Maybe PortELBLink
  } deriving (Eq, Show, Generic)

makeLenses ''PortMapping

instance FromJSON PortMapping where
  parseJSON = withObject "port mapping" $ \o -> do
    _pmContainerPort <- o .: "container-port"
    _pmHostPort      <- o .:? "host-port"
    _pmProtocol      <- o .:? "protocol"

    -- Load balancer link
    let nameLink        = ELBNameLink     <$> (MaybeT $ o .:? "lb-name")
    let targetGroupLink = TargetGroupLink <$> (MaybeT $ o .:? "target-group")
    _pmElbLink       <- runMaybeT $ nameLink <|> targetGroupLink

    return PortMapping{..}

newtype SharedMemory = SharedMemory Int
  deriving (Eq, Show, Generic)

newtype ReservedMemory = ReservedMemory Int
  deriving (Eq, Show, Generic)

data Container = Container
  { _cName         :: Text
  , _cImage        :: Text
  , _cMemory       :: Maybe Int
  , _cCpu          :: Maybe Int
  , _cPortMappings :: [PortMapping]
  , _cEnvironment  :: HashMap Text Text
  , _cLogConfig    :: Maybe ECS.LogConfiguration
  } deriving (Eq, Show, Generic)

makeLenses ''Container

instance FromJSON Container where
  parseJSON = withObject "container" $ \o -> do
    _cName         <- T.pack <$> o .: "name"
    _cImage        <- T.pack <$> o .: "image"
    _cMemory       <- o .:? "memory"
    _cCpu          <- o .:? "cpu"
    _cPortMappings <- maybe [] id <$> o .:? "port-mappings"
    _cEnvironment  <- maybe Map.empty id <$> o .:? "environment"
    _cLogConfig    <- o .:? "logging"
    return Container{..}

data DeploymentStrategy =
    DSBlueGreen
  | DSRolling
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

defaultDeploymentStrategy :: DeploymentStrategy
defaultDeploymentStrategy = DSBlueGreen

instance FromJSON DeploymentStrategy where
  parseJSON = withText "deployment strategy" $ \txt ->
    case (T.toLower txt) of
      "blue-green" -> return DSBlueGreen
      "rolling"    -> return DSRolling
      _            -> fail $ "Invalid deployment strategy: " ++ (T.unpack txt)

data ServiceDeployment = ServiceDeployment
  { _sdName               :: Text
  , _sdTaskRole           :: Maybe Text
  , _sdServiceRole        :: Maybe Text
  , _sdDesiredCount       :: Int
  , _sdDeploymentStrategy :: DeploymentStrategy
  , _sdContainers         :: [Container]
  , _sdNetworkMode        :: Maybe ECS.NetworkMode
  } deriving (Eq, Show, Generic)

makeLenses ''ServiceDeployment

instance FromJSON ServiceDeployment where
  parseJSON = withObject "service deployment" $ \o -> do
    _sdName               <- o .: "name"
    _sdTaskRole           <- o .:? "task-role"
    _sdServiceRole        <- o .:? "service-role"
    _sdDesiredCount       <- maybe 1 id <$> o .:? "desired-count"
    _sdDeploymentStrategy <- maybe defaultDeploymentStrategy id <$> o .:? "deployment-strategy"
    _sdContainers         <- o .: "containers"
    _sdNetworkMode        <- o .:? "network"
    return ServiceDeployment{..}

data GrootCompose = GrootCompose [ServiceDeployment]
  deriving (Eq, Show, Generic)

instance FromJSON GrootCompose where
  parseJSON = genericParseJSON defaultOptions {
                 fieldLabelModifier = drop 1 }

-- Validates that the given id points to an active cluster
verifyClusterIsActive :: (MonadResource m, MonadBaseControl IO m)
                      => ClusterRef
                      -> GrootM m ()
verifyClusterIsActive clusterRef = do
  env <- ask
  runAWS env $ do
    mcluster <- runMaybeT $ filterM isActiveCluster (findCluster clusterRef)
    case mcluster of
      Nothing -> throwM $ invalidClusterStatus clusterRef CSInactive (Just CSActive)
      Just  _ -> return ()

serviceExists :: (MonadResource m, MonadBaseControl IO m)
              => ClusterRef
              -> ContainerServiceRef
              -> GrootM m Bool
serviceExists clusterRef serviceRef = do
  env <- ask
  hoist (runAWS env) $ awsToGrootM $ (maybe False (const True) <$> check)
  where check = runMaybeT $ filterM isActiveContainerService (findService serviceRef (Just clusterRef))

createTaskDefinitionReq :: ServiceDeployment -> ECS.RegisterTaskDefinition
createTaskDefinitionReq deployment =
  ECS.rtdNetworkMode .~ (deployment ^. sdNetworkMode) $
  ECS.rtdTaskRoleARN .~ (deployment ^. sdTaskRole) $
  ECS.rtdContainerDefinitions .~ (containerDef <$> deployment ^. sdContainers) $
  ECS.registerTaskDefinition (toText $ deployment ^. sdName)
  where containerEnv env =
          Map.foldrWithKey (\k v acc -> (ECS.kvpName ?~ k $ ECS.kvpValue ?~ v $ ECS.keyValuePair):acc) [] env

        containerPort port =
          ECS.pmProtocol .~ (port ^. pmProtocol) $
          ECS.pmHostPort .~ (port ^. pmHostPort) $
          ECS.pmContainerPort ?~ (port ^. pmContainerPort) $
          ECS.portMapping

        containerDef c =
          ECS.cdImage ?~ (c ^. cImage) $
          ECS.cdEnvironment .~ (containerEnv $ c ^. cEnvironment) $
          ECS.cdPortMappings .~ (containerPort <$> c ^. cPortMappings) $
          ECS.cdMemory .~ (c ^. cMemory) $
          ECS.cdCpu .~ (c ^. cCpu) $
          ECS.cdLogConfiguration .~ (c ^. cLogConfig) $
          ECS.cdName ?~ (c ^. cName) $
          ECS.containerDefinition

serviceDeploymentConf :: DeploymentStrategy -> ECS.DeploymentConfiguration
serviceDeploymentConf DSBlueGreen =
  ECS.dcMinimumHealthyPercent ?~ 100 $
  ECS.dcMaximumPercent ?~ 200 $
  ECS.deploymentConfiguration
serviceDeploymentConf DSRolling =
  ECS.dcMinimumHealthyPercent ?~ 50 $
  ECS.dcMaximumPercent ?~ 200 $
  ECS.deploymentConfiguration

createServiceReq :: ClusterRef -> ServiceDeployment -> TaskDefId -> ECS.CreateService
createServiceReq clusterRef deployment tdId =
  ECS.cCluster ?~ (toText clusterRef) $
  ECS.cRole .~ (deployment ^. sdServiceRole) $
  ECS.cLoadBalancers .~ containerLoadBalancers $
  ECS.cDeploymentConfiguration ?~ (serviceDeploymentConf $ deployment ^. sdDeploymentStrategy) $
  ECS.createService (deployment ^. sdName) (toText tdId) (deployment ^. sdDesiredCount)
  where loadBalancerConf container pm =
          let linkElb lnk =
                ECS.lbContainerName ?~ (container ^. cName) $
                ECS.lbContainerPort ?~ (pm ^. pmContainerPort) $
                assignLink $
                ECS.loadBalancer
                where assignLink = case lnk of
                        ELBNameLink     x -> ECS.lbLoadBalancerName ?~ x
                        TargetGroupLink x -> ECS.lbTargetGroupARN   ?~ x
          in linkElb <$> (pm ^. pmElbLink)

        containerLoadBalancers = catMaybes $ do
          cont <- deployment ^. sdContainers
          pm   <- cont ^. cPortMappings
          return $ loadBalancerConf cont pm

updateServiceReq :: ClusterRef -> ServiceDeployment -> TaskDefId -> ECS.UpdateService
updateServiceReq clusterRef deployment tdId =
  ECS.usCluster ?~ (toText clusterRef) $
  ECS.usTaskDefinition ?~ (toText tdId) $
  ECS.usDesiredCount ?~ (deployment ^. sdDesiredCount) $
  ECS.usDeploymentConfiguration ?~ (serviceDeploymentConf $ deployment ^. sdDeploymentStrategy) $
  ECS.updateService (deployment ^. sdName)

deregisterTaskDefinitions :: (MonadResource m, MonadBaseControl IO m)
                          => Env
                          -> [TaskDefArn]
                          -> m ()
deregisterTaskDefinitions env arns = forM_ arns deregisterSingle
  where deregisterSingle x = do
          _ <- runAWS env . send $ ECS.deregisterTaskDefinition $ toText x
          return ()

registerTasks :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadReader e m, HasEnv e)
              => [ServiceDeployment]
              -> m [(ServiceDeployment, TaskDefId)]
registerTasks services = runResourceT $ execStateT (traverse registerSingle services) []
  where registerSingle :: (MonadResource m, MonadBaseControl IO m, MonadReader e m, HasEnv e)
                       => ServiceDeployment
                       -> StateT [(ServiceDeployment, TaskDefId)] m ()
        registerSingle dep = do
          env   <- lift $ ask
          liftIO . putInfo $ "Registering task definition: " <> (dep ^. sdName)
          res   <- lift $ runAWS env . send $ createTaskDefinitionReq dep
          mtask <- pure $ res ^. ECS.rtdrsTaskDefinition >>= taskDefId
          case mtask of
            Nothing   -> throwM $ failedToRegisterTaskDef (TaskDefRef $ dep ^. sdName)
            Just task -> do
              prev <- get
              put ((dep,task):prev)

deployServices :: (MonadResource m, MonadBaseControl IO m)
               => ClusterRef
               -> [(ServiceDeployment, TaskDefId)]
               -> GrootM m ()
deployServices clusterRef = void . traverse (\(dep, tskId) -> deploySingle dep tskId)
  where deploySingle :: (MonadResource m, MonadBaseControl IO m)
                     => ServiceDeployment
                     -> TaskDefId
                     -> GrootM m ()
        deploySingle deployment tdId = do
          env      <- ask
          exists   <- serviceExists clusterRef $ ContainerServiceRef (deployment ^. sdName)
          mservice <- runAWS env $
            if exists then do
              liftIO . putInfo $ "Updating service: " <> (deployment ^. sdName)
              fmap (\x -> x ^. ECS.usrsService) . send $ updateServiceReq clusterRef deployment tdId
            else do
              liftIO . putInfo $ "Creating service: " <> (deployment ^. sdName)
              fmap (\x -> x ^. ECS.csrsService) . send $ createServiceReq clusterRef deployment tdId
          case mservice of
            Nothing -> throwM $ failedServiceDeployment (ContainerServiceRef (deployment ^. sdName)) clusterRef
            Just  _ -> return ()

composeServices :: GrootCompose -> ClusterRef -> GrootM IO ()
composeServices (GrootCompose services) clusterRef = hoist runResourceT $ do
  verifyClusterIsActive clusterRef
  registered <- registerTasks services
  deployServices clusterRef registered
  liftIO . print $ snd <$> registered
