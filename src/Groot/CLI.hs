{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Groot.CLI
Description : Command Line Interface for Groot
Copyright   : A. Alonso Dominguez (c) 2017, http://github.com/alonsodomin
License     : Apache 2.0
Maintainer  : A. Alonso Dominguez <alonso.domin (λ) google>
Stability   : experimental
Portability : portable

This is the main application module, application's 'main' function hooks into this
via the 'grootCli' function and applications interested on embedding some of Groot's
features can use some of the more fain grained functions exposed via this module.
-}
module Groot.CLI
     ( GrootOpts
     , GrootCmd
     , loadEnv
     , grootOpts
     , grootCli
     , evalCmd
     ) where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Semigroup             ((<>))
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Version               (showVersion)
import           Network.AWS                hiding (Debug)
import qualified Network.AWS                as AWS
import           Options.Applicative
import           Paths_groot                (version)
import           System.IO

import           Groot.CLI.Cluster
import           Groot.CLI.Common
import           Groot.CLI.Introspect
import           Groot.CLI.List
import           Groot.CLI.Service
import           Groot.Config
import           Groot.Console
import           Groot.Core
import           Groot.Exception
import           Groot.Internal.Data.Text
import           Groot.Manifest

credsOpt :: Parser Credentials
credsOpt = fromContainer <|> fromSession <|> fromKeys <|> fromEnv <|> fromProfile <|> fromFile <|> (pure Discover)
  where
    -- Constructors
    fromContainer = flag' FromContainer (long "from-container" <> help "Use ECS Container credentials")
    fromKeys      = FromKeys <$> accessKey <*> secretKey
    fromSession   = FromSession <$> accessKey <*> secretKey <*> ((SessionToken . fromString) <$> sessionToken)
    fromProfile   = FromProfile <$> iamProfile
    fromFile      = FromFile <$> profile <*> file
    fromEnv       = FromEnv <$> keyEnv <*> secretEnv <*> (optional $ T.pack <$> sessionToken) <*> (optional region)

    -- Individual items
    profile = T.pack <$> strOption
            ( long "profile"
           <> short 'p'
           <> metavar "AWS_PROFILE"
           <> help "AWS Profile" )

    iamProfile = T.pack <$> strOption
           ( long "iam-profile"
          <> metavar "AWS_IAM_PROFILE"
          <> help "AWS IAM Profile" )

    accessKey = (AccessKey . fromString) <$> strOption
              ( long "access-key"
             <> metavar "ACCESS_KEY"
             <> help "AWS Access Key" )

    secretKey = (SecretKey . fromString) <$> strOption
              ( long "secret-key"
             <> metavar "SECRET_KEY"
             <> help "AWS Secret Key" )

    sessionToken = strOption
                 ( long "session-token"
                <> metavar "SESSION_TOKEN"
                <> help "AWS Session Token" )

    file = strOption
         ( long "creds"
        <> metavar "CRENDENTIALS_FILE"
        <> help "AWS Credentials config file"
        <> value "~/.aws/credentials" )

    keyEnv = T.pack <$> strOption
        ( long "access-key-var"
       <> metavar "ACCESS_KEY_VAR"
       <> help "AWS Access Key Environment variable" )

    secretEnv = T.pack <$> strOption
           ( long "secret-key-var"
          <> metavar "SECRET_KEY_VAR"
          <> help "AWS Secret Key Environment variable" )

    region = T.pack <$> strOption
        ( long "region"
       <> short 'r'
       <> metavar "REGION"
       <> help "AWS Region" )

regionOpt :: Parser Region
regionOpt = option (attoReadM parser)
          ( long "region"
          <> short 'r'
          <> metavar "AWS_REGION"
          <> help "AWS Region identifier" )

versionInfo :: String
versionInfo = "groot " ++ (showVersion version)

versionOpt :: Parser (a -> a)
versionOpt = infoOption versionInfo $ mconcat [
    long "version"
  , short 'v'
  , help "Show version number"
  ]

-- |Top level Groot commands
data GrootCmd =
    ClusterCmd ClusterSubCmd
  | ListCmd ListSubCmd
  | ServiceCmd ServiceSubCmd
  | IntrospectCmd IntrospectOpts
  deriving (Eq, Show)

-- |Groot options for a given execution
data GrootOpts = GrootOpts
  { _grootCreds  :: Credentials
  , _grootRegion :: Maybe Region
  , _grootDebug  :: Bool
  , _grootCmd    :: GrootCmd
  } deriving Eq

makeLenses ''GrootOpts

introspectCmd :: Parser GrootCmd
introspectCmd = IntrospectCmd <$> introspectOpts

commands :: Parser GrootCmd
commands = hsubparser
   ( command "ls"         (info (ListCmd    <$> listCmds)    (progDesc "List ECS resources"))
  <> command "cluster"    (info (ClusterCmd <$> clusterCmds) (progDesc "Perform cluster related operations"))
  <> command "service"    (info (ServiceCmd <$> serviceCmds) (progDesc "Perform service related operations"))
  <> command "introspect" (info introspectCmd                (progDesc "Introspect the manifest from a running cluster"))
  )

-- |Command line parser for all the possible Groot options
grootOpts :: Parser GrootOpts
grootOpts = ( GrootOpts
          <$> credsOpt
          <*> optional regionOpt
          <*> switch
            ( long "debug"
           <> help "Enable debug mode when running" )
          <*> commands
          ) <**> versionOpt

-- | Generates the AWS Env from the command line options
loadEnv :: GrootOpts -> IO Env
loadEnv opts = do
  env <- newEnv $ opts ^. grootCreds
  assignRegion (opts ^. grootRegion) <$> setupLogger env
    where assignRegion :: Maybe Region -> Env -> Env
          assignRegion maybeRegion env =
            maybe id (\x -> envRegion .~ x) maybeRegion env

          setupLogger :: Env -> IO Env
          setupLogger env = if opts ^. grootDebug
            then do
              lgr <- newLogger AWS.Debug stdout
              return $ env & envLogger .~ lgr
            else return env

-- Groot Error handlers

handleClusterNotFound :: ClusterNotFound -> IO ()
handleClusterNotFound (ClusterNotFound' clusterRef) =
  putError $ "Could not find cluster " <> (styled yellowStyle $ toText clusterRef)

handleInvalidClusterStatus :: InvalidClusterStatus -> IO ()
handleInvalidClusterStatus (InvalidClusterStatus' clusterRef currentSt desiredSt) =
  putError $ "Can't operate on cluster " <> (styled yellowStyle $ toText clusterRef)
    <> " because it is " <> (styled redStyle $ toText currentSt)
    <> maybe "." (\x -> ", it should be " <> (styled greenStyle $ toText x) <> " to continue.") desiredSt

handleServiceNotFound :: ServiceNotFound -> IO ()
handleServiceNotFound (ServiceNotFound' serviceRef clusterRef) =
  putError $ "Could not find service " <> (styled yellowStyle $ toText serviceRef) <>
    maybe "" (\x -> " in cluster " <> (styled yellowStyle $ toText x)) clusterRef

handleAmbiguousServiceName :: AmbiguousServiceName -> IO ()
handleAmbiguousServiceName (AmbiguousServiceName' serviceRef clusters) =
  let stringifyClusters = styleless $ "\n - " <> (T.intercalate "\n - " $ toText <$> clusters)
  in putError $ "Service name " <> (styled yellowStyle $ toText serviceRef)
       <> " is ambiguous. It was found in the following clusters:" <> stringifyClusters

handleInactiveService :: InactiveService -> IO ()
handleInactiveService (InactiveService' serviceRef clusterRef) =
  putError $ "Service " <> (styled yellowStyle $ toText serviceRef) <> " in cluster "
    <> (styled yellowStyle $ toText $ clusterRef)
    <> " is not active."

handleTaskNotFound :: TaskNotFound -> IO ()
handleTaskNotFound (TaskNotFound' taskRef clusterRef) =
  putError $ "Could not find task " <> (styled yellowStyle $ toText taskRef) <>
    maybe "" (\x -> " in cluster " <> (styled yellowStyle $ toText x)) clusterRef

-- Main Program execution

handleExceptions :: IO () -> IO ()
handleExceptions act = catches act [
    handler _TransportError          handleHttpException
  , handler _ServiceError            handleServiceError
  , handler _ClusterNotFound         handleClusterNotFound
  , handler _InvalidClusterStatus    handleInvalidClusterStatus
  , handler _ServiceNotFound         handleServiceNotFound
  , handler _AmbiguousServiceName    handleAmbiguousServiceName
  , handler _InactiveService         handleInactiveService
  , handler _TaskNotFound            handleTaskNotFound
  , handler _ManifestParseError      handleManifestParseError
  ]

-- |Runs a Groot command with the given AWS environment
evalCmd :: GrootCmd -> GrootIO ()
evalCmd (ClusterCmd opts)    = runClusterCmd opts
evalCmd (ListCmd opts)       = runListCmd opts
evalCmd (ServiceCmd opts)    = runServiceCmd opts
evalCmd (IntrospectCmd opts) = runIntrospect opts

-- |Groot main entry point from the Command line, able to interpret
-- arguments and parameters passed to it
grootCli :: IO ()
grootCli =
  prog =<< (execParser cli)
  where prog opts = do
          env <- loadEnv opts
          handleExceptions $ mainBlock (opts ^. grootCmd) env

        cli = info (grootOpts <**> helper)
          ( fullDesc
          <> progDesc "Utility to manage ECS Clusters"
          <> header "groot" )

        mainBlock cmd = runGroot (evalCmd cmd)
