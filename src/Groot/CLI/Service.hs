module Groot.CLI.Service
     ( ServiceSubCmd
     , serviceCmds
     , runServiceCmd
     ) where

import           Data.Semigroup            ((<>))
import           Options.Applicative

import           Groot.CLI.Service.Compose
import           Groot.CLI.Service.Events
import           Groot.CLI.Service.Inspect
import           Groot.Core

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

-- run function

runServiceCmd :: ServiceSubCmd -> GrootM IO ()
runServiceCmd (ServiceEventsCmd eventsOpts)   = runServiceEvents  eventsOpts
runServiceCmd (ServiceUpCmd     composeOpts)  = runServiceUp      composeOpts
runServiceCmd (ServiceDeleteCmd composeOpts)  = runServiceDelete  composeOpts
runServiceCmd (ServiceInspectCmd inspectOpts) = runServiceInspect inspectOpts
