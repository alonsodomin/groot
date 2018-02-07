{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Groot.Compose.Service.DryRun (dryRunServiceCompose) where

import           Control.Monad.Free
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Data.Semigroup               ((<>))
import           Network.AWS

import           Groot.Compose.Service.AWS (serviceExists')
import           Groot.Compose.Service.Free
import           Groot.Compose.Service.Model
import           Groot.Core
import           Groot.Console
import Groot.Data.Text
import Groot.Exception
import Groot.Types

registerTask' :: (MonadConsole m, MonadResource m) => NamedServiceDeployment -> GrootM m TaskDefId
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

createService' :: (MonadConsole m, MonadResource m)
               => NamedServiceDeployment
               -> ClusterRef
               -> TaskDefId
               -> GrootM m ()
createService' = undefined

updateService' :: (MonadConsole m, MonadResource m)
               => NamedServiceDeployment
               -> ClusterRef
               -> TaskDefId
               -> GrootM m ()
updateService' = undefined

dryRunServiceCompose :: (MonadConsole m, MonadResource m) => ServiceComposeM a -> GrootM m a
dryRunServiceCompose = foldFree $ \case
  RegisterTask service next ->
    next <$> registerTask' service
  ServiceExists name clusterRef next ->
    next <$> serviceExists' name clusterRef
  CreateService service cluster taskId next ->
    (const next) <$> createService' service cluster taskId
  UpdateService service cluster taskId next ->
    (const next) <$> updateService' service cluster taskId