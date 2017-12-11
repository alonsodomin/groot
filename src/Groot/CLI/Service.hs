module Groot.CLI.Service
     ( ServiceSubCmd
     , serviceCmds
     , runServiceCmd
     ) where

import           Network.AWS
import           Options.Applicative

import           Groot.CLI.Service.Events
import           Groot.Core

data ServiceSubCmd =
  ServiceEventsCmd ServiceEventOpts
  deriving (Eq, Show)

-- CLI

serviceEventsCmd :: Parser ServiceSubCmd
serviceEventsCmd = ServiceEventsCmd <$> serviceEventsOpt

serviceCmds :: Parser ServiceSubCmd
serviceCmds = hsubparser
  ( command "events" (info serviceEventsCmd (progDesc "Display events of the given services"))
  )

-- run function

runServiceCmd :: ServiceSubCmd -> GrootM IO ()
runServiceCmd (ServiceEventsCmd eventsOpts) = runServiceEvents eventsOpts
