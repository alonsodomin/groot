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
    ServiceEventsCmd ServiceEventOpts
  | ServiceComposeCmd ServiceComposeOpts
  deriving (Eq, Show)

-- CLI

serviceEventsCmd :: Parser ServiceSubCmd
serviceEventsCmd = ServiceEventsCmd <$> serviceEventsOpt

serviceComposeCmd :: Parser ServiceSubCmd
serviceComposeCmd = ServiceComposeCmd <$> serviceComposeOpts

serviceCmds :: Parser ServiceSubCmd
serviceCmds = hsubparser
  ( command "events"  (info serviceEventsCmd  (progDesc "Display events of the given services"))
 <> command "compose" (info serviceComposeCmd (progDesc "Manage service deployments"))
  )

-- run function

runServiceCmd :: ServiceSubCmd -> GrootM IO ()
runServiceCmd (ServiceEventsCmd eventsOpts) = runServiceEvents eventsOpts
runServiceCmd (ServiceComposeCmd composeOpts) = runServiceCompose composeOpts
