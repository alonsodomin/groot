{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Groot.Core.Compose where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Control.Monad.IO.Class
import           Control.Monad.Reader           hiding (filterM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as Map
import qualified Data.List.NonEmpty             as NEL
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
findActiveCluster :: ClusterRef -> GrootM IO ECS.Cluster
findActiveCluster clusterRef = do
  env <- ask
  runResourceT . runAWS env $ do
    mcluster <- runMaybeT $ filterM isActiveCluster (findCluster clusterRef)
    case mcluster of
      Nothing -> throwM $ invalidClusterStatus clusterRef CSInactive (Just CSActive)
      Just x  -> return x

findActiveService :: ContainerServiceRef -> ClusterRef -> GrootM IO ECS.ContainerService
findActiveService serviceRef clusterRef = do
  env <- ask
  runResourceT . runAWS env $ do
    mservice <- runMaybeT $ filterM isActiveContainerService (findService serviceRef (Just clusterRef))
    case mservice of
      Nothing -> throwM $ inactiveService serviceRef clusterRef
      Just x  -> return x

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
              -> m [(ServiceDeployment, ECS.TaskDefinition)]
registerTasks services = go
  where registerSingle :: (MonadResource m, MonadBaseControl IO m, MonadReader e m, HasEnv e)
                       => ServiceDeployment
                       -> StateT [(ServiceDeployment, ECS.TaskDefinition)] m ()
        registerSingle dep = do
          env   <- lift $ ask
          res   <- lift $ runAWS env . send $ createTaskDefinitionReq dep
          mtask <- pure $ res ^. ECS.rtdrsTaskDefinition
          case mtask of
            Nothing   -> throwM $ failedToRegisterTaskDef (TaskDefRef $ dep ^. sdName)
            Just task -> do
              prev <- get
              put ((dep,task):prev)

        go :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadReader e m, HasEnv e)
           => m [(ServiceDeployment, ECS.TaskDefinition)]
        go = runResourceT $ execStateT (traverse aroundRegisterSingle services) []
          where aroundRegisterSingle service = do
                  liftIO . putInfo $ "Registering task definition: " <> (service ^. sdName)
                  registerSingle service

        rollback :: [ECS.TaskDefinition] -> m ()
        rollback = undefined

composeServices :: GrootCompose -> ClusterRef -> GrootM IO ()
composeServices (GrootCompose services) clusterRef = do
  cluster <- findActiveCluster clusterRef
  registered <- registerTasks services
  liftIO . print $ snd <$> registered
