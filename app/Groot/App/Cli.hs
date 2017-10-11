module Groot.App.Cli
     (
       Cmd (..)
     , CliOptions (..)
     , module Groot.App.Cli.Parsers
     , runGroot
     ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Network.AWS (Credentials, Region)

import Groot.App.Cli.Parsers
import Groot.App.Compose
import Groot.App.Events
import Groot.App.List

data Cmd =
    ComposeCmd ComposeOptions
  | ListCmd ListOptions
  | EventsCmd EventOptions
  deriving (Eq, Show)

data CliOptions = CliOptions
  { awsCreds  :: AwsCredentials
  , awsRegion :: Maybe Region
  , cmd       :: Cmd
  } deriving Eq

commands :: Parser Cmd
commands = hsubparser
   ( command "ls"      (info (ListCmd    <$> grootListCli)    (progDesc "List ECS resources"))
  <> command "events"  (info (EventsCmd  <$> grootEventsCli)  (progDesc "Display events for a given ECS service"))
  <> command "compose" (info (ComposeCmd <$> grootComposeCli) (progDesc "Handle Groot compose files"))
   )

cliParser :: Parser CliOptions
cliParser = CliOptions
          <$> credsParser
          <*> optional regionParser
          <*> commands

runGroot :: (CliOptions -> IO ()) -> IO ()
runGroot prog =
  prog =<< (execParser opts)
  where opts = info (cliParser <**> helper)
          ( fullDesc
         <> progDesc "Utility to manage ECS Clusters"
         <> header "groot" )
