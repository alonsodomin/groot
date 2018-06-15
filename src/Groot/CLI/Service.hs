{-# LANGUAGE OverloadedStrings #-}

module Groot.CLI.Service
     ( ServiceSubCmd
     , serviceCmds
     , runServiceCmd
     ) where

import           Control.Exception.Lens
import           Control.Monad.Catch
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Options.Applicative

import           Groot.CLI.Service.Compose
import           Groot.CLI.Service.Events
import           Groot.CLI.Service.Inspect
import           Groot.Console
import           Groot.Core
import           Groot.Data.Text
import           Groot.Exception

data ServiceSubCmd =
    ServiceEventsCmd  ServiceEventOpts
  | ServiceUpCmd      ServiceComposeOpts
  | ServiceDeleteCmd  ServiceComposeOpts
  | ServiceInspectCmd ServiceInspectOpts
  deriving (Eq, Show)

-- CLI

serviceEventsCmd :: Parser ServiceSubCmd
serviceEventsCmd = ServiceEventsCmd <$> serviceEventsOpt

serviceUpCmd :: Parser ServiceSubCmd
serviceUpCmd = ServiceUpCmd <$> serviceComposeOpts

serviceDeleteCmd :: Parser ServiceSubCmd
serviceDeleteCmd = ServiceDeleteCmd <$> serviceComposeOpts

serviceInspectCmd :: Parser ServiceSubCmd
serviceInspectCmd = ServiceInspectCmd <$> serviceInspectOpts

serviceCmds :: Parser ServiceSubCmd
serviceCmds = hsubparser
  ( command "events"  (info serviceEventsCmd  (progDesc "Display events of the given services."))
 <> command "up"      (info serviceUpCmd      (progDesc "Deploy services as stated in a service file."))
 <> command "rm"      (info serviceDeleteCmd  (progDesc "Delete previously deployed services."))
 <> command "inspect" (info serviceInspectCmd (progDesc "Inspect details of a given service."))
  )

-- Error handlers

handleUndefinedService :: MonadConsole m => UndefinedService -> m ()
handleUndefinedService (UndefinedService' serviceName manifestFile) =
  putError $ "Service" <+> (styled yellowStyle serviceName)
    <+> "has not been defined in compose file:"
    <+> (styled yellowStyle $ T.pack manifestFile)

handleDeploymentFailed :: MonadConsole m => FailedServiceDeployment -> m ()
handleDeploymentFailed (FailedServiceDeployment' serviceRef clusterRef reason) =
  putError $ "Failed to deploy service" <+> (styled yellowStyle $ toText serviceRef)
    <+> "in cluster" <+> (styled yellowStyle $ toText clusterRef)
    <> (maybe "" (\x -> " because" <+> (styled yellowStyle x)) reason)

handleDeletionFailed :: MonadConsole m => FailedServiceDeletion -> m ()
handleDeletionFailed (FailedServiceDeletion' serviceRef clusterRef) =
  putError $ "Failed to delete service" <+> (styled yellowStyle $ toText serviceRef)
    <+> "from cluster" <+> (styled yellowStyle $ toText clusterRef)

handleErrors :: GrootIO () -> GrootIO ()
handleErrors act = catches act [
    handler _UndefinedService        handleUndefinedService
  , handler _FailedServiceDeployment handleDeploymentFailed
  , handler _FailedServiceDeletion   handleDeletionFailed
  ]

-- run function

runServiceCmd :: ServiceSubCmd -> GrootIO ()
runServiceCmd (ServiceEventsCmd eventsOpts)   = runServiceEvents  eventsOpts
runServiceCmd (ServiceUpCmd     composeOpts)  = handleErrors $ runServiceUp      composeOpts
runServiceCmd (ServiceDeleteCmd composeOpts)  = handleErrors $ runServiceDelete  composeOpts
runServiceCmd (ServiceInspectCmd inspectOpts) = runServiceInspect inspectOpts
