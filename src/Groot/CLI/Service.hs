module Groot.CLI.Service
     ( ServiceSubCmd
     , serviceCmds
     , runServiceCmd
     ) where

import           Data.Semigroup            ((<>))
import           Network.AWS
import           Options.Applicative

import           Groot.CLI.Service.Compose
import           Groot.CLI.Service.Events
import           Groot.Core

data ServiceSubCmd =
    ServiceEventsCmd  ServiceEventOpts
  | ServiceComposeCmd ServiceComposeOpts
  | ServiceDeleteCmd  ServiceComposeOpts
  deriving (Eq, Show)

-- CLI

serviceEventsCmd :: Parser ServiceSubCmd
serviceEventsCmd = ServiceEventsCmd <$> serviceEventsOpt

serviceComposeCmd :: Parser ServiceSubCmd
serviceComposeCmd = ServiceComposeCmd <$> serviceComposeOpts

serviceDeleteCmd :: Parser ServiceSubCmd
serviceDeleteCmd = ServiceDeleteCmd <$> serviceComposeOpts

serviceCmds :: Parser ServiceSubCmd
serviceCmds = hsubparser
  ( command "events"  (info serviceEventsCmd  (progDesc "Display events of the given services"))
 <> command "compose" (info serviceComposeCmd (progDesc "Manage service deployments"))
 <> command "rm"      (info serviceDeleteCmd  (progDesc "Delete previously deployed services"))
  )

-- run function

runServiceCmd :: ServiceSubCmd -> GrootM IO ()
runServiceCmd (ServiceEventsCmd  eventsOpts)  = runServiceEvents  eventsOpts
runServiceCmd (ServiceComposeCmd composeOpts) = runServiceCompose composeOpts
runServiceCmd (ServiceDeleteCmd  composeOpts) = runServiceDelete  composeOpts
