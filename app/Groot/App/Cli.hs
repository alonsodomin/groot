module Groot.App.Cli
     (
       Cmd (..)
     , CliOptions (..)
     , module Groot.App.Cli.Parsers
     , runGroot
     ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Network.AWS (Region)

import Groot.App.Cli.Parsers
import Groot.App.Compose
import Groot.App.Service
import Groot.App.List
import Groot.App.Task

data Cmd =
    ComposeCmd ComposeOptions
  | ListCmd ListOptions
  | ServiceCmd ServiceOptions
  | TaskCmd TaskOptions
  deriving (Eq, Show)

data CliOptions = CliOptions
  { awsCreds  :: AwsCredentials
  , awsRegion :: Maybe Region
  , cmd       :: Cmd
  } deriving Eq

commands :: Parser Cmd
commands = hsubparser
   ( command "ls"      (info (ListCmd    <$> grootListCli)    (progDesc "List ECS resources"))
  <> command "service" (info (ServiceCmd <$> grootServiceCli) (progDesc "Perform service related operations"))
  <> command "compose" (info (ComposeCmd <$> grootComposeCli) (progDesc "Handle Groot compose files"))
  <> command "task"    (info (TaskCmd    <$> grootTaskCli)    (progDesc "Manage ECS tasks"))
   )

cliParser :: Parser CliOptions
cliParser = CliOptions
          <$> credsOpt
          <*> optional regionOpt
          <*> commands

runGroot :: (CliOptions -> IO ()) -> IO ()
runGroot prog =
  prog =<< (execParser opts)
  where opts = info (cliParser <**> helper)
          ( fullDesc
         <> progDesc "Utility to manage ECS Clusters"
         <> header "groot" )
