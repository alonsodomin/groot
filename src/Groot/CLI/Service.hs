module Groot.App.Service
     ( ServiceSubCmd
     , serviceCmds
     , runServiceCmd
     ) where

import Network.AWS
import Options.Applicative

import Groot.CLI.Service.Events

data ServiceSubCmd =
  ServiceEventsCmd ServiceEventOptions
  deriving (Eq, Show)

-- CLI

serviceEventsCmd :: Parser ServiceSubCmd
serviceEventsCmd = ServiceEventsCmd <$> serviceEventsCli

serviceCmds :: Parser ServiceCmd
serviceCmds = hsubparser
  ( command "events" (info serviceEventsCmd (progDesc "Display events of the given services"))
  )

-- run function

runServiceCmd :: ServiceSubCmd -> Env -> IO ()
runServiceCmd (ServiceEventsCmd eventsOpts) = runServiceEvents eventsOpts