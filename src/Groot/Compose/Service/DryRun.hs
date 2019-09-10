{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.Compose.Service.DryRun (runServiceCompose) where

import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Semigroup               ((<>))
import           Network.AWS

import           Groot.Compose.Service.AWS    (serviceExists',
                                               verifyActiveCluster')
import           Groot.Compose.Service.API
import           Groot.Console
import           Groot.Core
import           Groot.Internal.Data.Text
import           Groot.Manifest
import           Groot.Types

registerTask' :: (MonadConsole m, MonadResource m)
              => NamedServiceDeployment
              -> GrootT m TaskDefId
registerTask' (serviceName, _) = do
  putInfo $ "Registering task definition for service " <> styled yellowStyle serviceName
  env  <- ask
  tdId <- runAWS env . runMaybeT $ do
    taskDef <- getTaskDef (TaskDefRef serviceName)
    MaybeT . pure $ nextTaskDefId <$> taskDefId taskDef
  case tdId of
    Nothing    -> do
      let theId = TaskDefId (TaskFamily serviceName) 0
      putSuccess $ "Would have created task" <+> (styled yellowStyle $ toText theId)
      return theId
    Just theId -> do
      putSuccess $ "Would have upgrade task to" <+> (styled yellowStyle $ toText theId)
      return theId

createService' :: MonadConsole m
               => NamedServiceDeployment
               -> ClusterRef
               -> TaskDefId
               -> m ()
createService' (serviceName, _) clusterRef tdId =
  putInfo $ "Creates a new service named" <+> (styled yellowStyle serviceName)
    <+> "in cluster" <+> (styled yellowStyle $ toText clusterRef)
    <+> "linked to task" <+> (styled yellowStyle $ toText tdId)

updateService' :: MonadConsole m
               => NamedServiceDeployment
               -> ClusterRef
               -> TaskDefId
               -> m ()
updateService' (serviceName, _) clusterRef tdId =
  putInfo $ "Updates service named" <+> (styled yellowStyle serviceName)
    <+> "in cluster" <+> (styled yellowStyle $ toText clusterRef)
    <+> "linked to task" <+> (styled yellowStyle $ toText tdId)

removeService' :: MonadConsole m
               => NamedServiceDeployment
               -> ClusterRef
               -> m ()
removeService' (serviceName, _) clusterRef =
  putInfo $ "Deletes service named" <+> (styled yellowStyle serviceName)
    <+> "from cluster" <+> (styled yellowStyle $ toText clusterRef)

runServiceCompose :: (MonadConsole m, MonadResource m)
                  => GrootManifest
                  -> ServiceComposeM a
                  -> GrootT m a
runServiceCompose _ = foldFree $ \case
  RegisterTask service next ->
    next <$> registerTask' service
  ServiceExists name clusterRef next ->
    next <$> serviceExists' name clusterRef
  CreateService service cluster taskId next ->
    (const next) <$> createService' service cluster taskId
  UpdateService service cluster taskId next ->
    (const next) <$> updateService' service cluster taskId
  RemoveService service cluster next ->
    (const next) <$> removeService' service cluster
  VerifyActiveCluster cluster next ->
    (const next) <$> verifyActiveCluster' cluster
