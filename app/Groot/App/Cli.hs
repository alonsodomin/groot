{-# LANGUAGE LambdaCase #-}

module Groot.App.Cli
     (
       Cmd (..)
     , CliOptions
     , cliAwsCreds
     , cliAwsRegion
     , cliCmd
     , module Groot.App.Cli.Parsers
     , runGroot
     ) where

import Control.Lens
import Data.Monoid
import Data.Version (showVersion)
import Network.AWS (Region)
import Options.Applicative
import Paths_groot (version)

import Groot.App.Cli.Parsers
import Groot.App.Compose
import Groot.App.Cluster
import Groot.App.Service
import Groot.App.List
import Groot.App.Task

data Cmd =
    ComposeCmd ComposeOptions
  | ClusterCmd ClusterOptions
  | ListCmd ListOptions
  | ServiceCmd ServiceOptions
  | TaskCmd TaskOptions
  deriving (Eq, Show)

data CliOptions = CliOptions
  { _cliAwsCreds  :: AwsCredentials
  , _cliAwsRegion :: Maybe Region
  , _cliCmd       :: Cmd
  } deriving Eq

cliAwsCreds :: Getter CliOptions AwsCredentials
cliAwsCreds = to _cliAwsCreds

cliAwsRegion :: Getting (First Region) CliOptions Region
cliAwsRegion = (to _cliAwsRegion) . _Just

cliCmd :: Getter CliOptions Cmd
cliCmd = to _cliCmd

-- CLI Parsers

commands :: Parser Cmd
commands = hsubparser
   ( command "ls"      (info (ListCmd    <$> grootListCli)    (progDesc "List ECS resources"))
  <> command "cluster" (info (ClusterCmd <$> grootClusterCli) (progDesc "Perform cluster related operations"))
  <> command "service" (info (ServiceCmd <$> grootServiceCli) (progDesc "Perform service related operations"))
  <> command "compose" (info (ComposeCmd <$> grootComposeCli) (progDesc "Handle Groot compose files"))
  <> command "task"    (info (TaskCmd    <$> grootTaskCli)    (progDesc "Manage ECS tasks"))
   )

cliVersionParser :: Parser (a -> a)
cliVersionParser = infoOption versionInfo $ mconcat [
    long "version"
  , short 'v'
  , help "Show version number"
  ]

cliParser :: Parser CliOptions
cliParser = ( CliOptions
          <$> credsOpt
          <*> optional regionOpt
          <*> commands
          ) <**> cliVersionParser

versionInfo :: String
versionInfo = "groot " ++ (showVersion version)

runGroot :: (CliOptions -> IO ()) -> IO ()
runGroot prog =
  prog =<< (execParser opts)
  where opts = info (cliParser <**> helper)
          ( fullDesc
         <> progDesc "Utility to manage ECS Clusters"
         <> header "groot" )
