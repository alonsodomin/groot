module Groot.CLI.Service
     ( ServiceSubCmd
     , serviceCmds
     , runServiceCmd
     ) where

import           Data.Semigroup            ((<>))
import           Options.Applicative

import           Groot.CLI.Service.Compose
import           Groot.CLI.Service.Events
import           Groot.Core

data ServiceSubCmd =
    ServiceEventsCmd ServiceEventOpts
  | ServiceUpCmd     ServiceComposeOpts
  | ServiceDeleteCmd ServiceComposeOpts
  deriving (Eq, Show)

-- CLI

serviceEventsCmd :: Parser ServiceSubCmd
serviceEventsCmd = ServiceEventsCmd <$> serviceEventsOpt

serviceUpCmd :: Parser ServiceSubCmd
serviceUpCmd = ServiceUpCmd <$> serviceComposeOpts

serviceDeleteCmd :: Parser ServiceSubCmd
serviceDeleteCmd = ServiceDeleteCmd <$> serviceComposeOpts

serviceCmds :: Parser ServiceSubCmd
serviceCmds = hsubparser
  ( command "events"  (info serviceEventsCmd (progDesc "Display events of the given services"))
 <> command "up"      (info serviceUpCmd     (progDesc "Deploy services as stated in a service file."))
 <> command "rm"      (info serviceDeleteCmd (progDesc "Delete previously deployed services"))
  )

-- run function

runServiceCmd :: ServiceSubCmd -> GrootM IO ()
runServiceCmd (ServiceEventsCmd eventsOpts)  = runServiceEvents eventsOpts
runServiceCmd (ServiceUpCmd     composeOpts) = runServiceUp     composeOpts
runServiceCmd (ServiceDeleteCmd composeOpts) = runServiceDelete composeOpts
