{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.Compose.Service.DryRun (dryRunServiceCompose) where

import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Semigroup               ((<>))
import           Network.AWS

import           Groot.Compose.Service.AWS    (serviceExists',
                                               verifyActiveCluster')
import           Groot.Compose.Service.Free
import           Groot.Compose.Service.Model
import           Groot.Console
import           Groot.Core
import           Groot.Data.Text
import           Groot.Exception
import           Groot.Types

registerTask' :: (MonadConsole m, MonadResource m, MonadBaseControl IO m)
              => NamedServiceDeployment
              -> GrootM m TaskDefId
registerTask' (serviceName, _) = do
  putInfo $ "Registering task definition for service " <> styled yellowStyle serviceName
  env  <- ask
  tdId <- runAWS env . runMaybeT $ do
    taskDef <- getTaskDef (TaskDefRef serviceName)
    MaybeT . pure $ taskDefId taskDef
  case tdId of
    Nothing    -> throwM $ failedToRegisterTaskDef (TaskDefRef serviceName)
    Just theId -> do
      putSuccess $ "Would have registered task" <+> (styled yellowStyle $ toText theId)
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

dryRunServiceCompose :: (MonadConsole m, MonadResource m, MonadBaseControl IO m)
                     => ServiceComposeM a
                     -> GrootM m a
dryRunServiceCompose = foldFree $ \case
  RegisterTask service next ->
    next <$> registerTask' service
  ServiceExists name clusterRef next ->
    next <$> serviceExists' name clusterRef
  CreateService service cluster taskId next ->
    (const next) <$> createService' service cluster taskId
  UpdateService service cluster taskId next ->
    (const next) <$> updateService' service cluster taskId
  VerifyActiveCluster cluster next ->
    (const next) <$> verifyActiveCluster' cluster
