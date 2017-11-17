module Groot.App.Service
     ( ServiceOptions
     , grootServiceCli
     , runGrootService
     ) where

import Network.AWS
import Options.Applicative

import Groot.App.Service.Events

data ServiceCmd =
  ServiceEventsCmd ServiceEventOptions
  deriving (Eq, Show)

data ServiceOptions = ServiceOptions ServiceCmd
  deriving (Eq, Show)

-- CLI

serviceEventsCmd :: Parser ServiceCmd
serviceEventsCmd = ServiceEventsCmd <$> serviceEventsCli

serviceCmds :: Parser ServiceCmd
serviceCmds = hsubparser
  ( command "events" (info serviceEventsCmd (progDesc "Display events of the given services"))
  )

grootServiceCli :: Parser ServiceOptions
grootServiceCli = ServiceOptions <$> serviceCmds

-- run function

runGrootService :: ServiceOptions -> Env -> IO ()
runGrootService (ServiceOptions (ServiceEventsCmd eventsOpts)) = runServiceEvents eventsOpts