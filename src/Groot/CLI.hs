{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Groot.CLI
     ( grootCli
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
import           Network.AWS
import           Options.Applicative
import           Paths_groot                (version)

import           Groot.CLI.Cluster
import           Groot.CLI.Common
import           Groot.CLI.List
import           Groot.CLI.Service
import           Groot.Config
import           Groot.Console
import           Groot.Core
import           Groot.Data.Text
import           Groot.Exception

data CredentialsOpt =
    ProfileOpt (Maybe Text) (Maybe FilePath)
  | KeysOpt AccessKey SecretKey
  deriving Eq

credsOpt :: Parser CredentialsOpt
credsOpt =
  let profile = T.pack <$> strOption
                ( long "profile"
                <> short 'p'
                <> metavar "AWS_PROFILE"
                <> help "AWS Profile" )
      accessKey = (AccessKey . fromString) <$> strOption
                  ( long "accessKey"
                  <> metavar "ACCESS_KEY"
                  <> help "AWS Access Key" )
      secretKey = (SecretKey . fromString) <$> strOption
                  ( long "secretKey"
                  <> metavar "SECRET_KEY"
                  <> help "AWS Secret Key" )
      file = strOption
              ( long "creds"
            <> metavar "CRENDENTIALS_FILE"
            <> help "AWS Credentials config file" )
  in let fromProfile = ProfileOpt <$> (optional profile) <*> (optional file)
         fromKeys    = KeysOpt <$> accessKey <*> secretKey
      in fromKeys <|> fromProfile

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

data GrootCmd =
    ClusterCmd ClusterSubCmd
  | ListCmd ListSubCmd
  | ServiceCmd ServiceSubCmd
  deriving (Eq, Show)

data GrootOpts = GrootOpts
  { _grootCreds  :: CredentialsOpt
  , _grootRegion :: Maybe Region
  , _grootCmd    :: GrootCmd
  } deriving Eq

makeLenses ''GrootOpts

commands :: Parser GrootCmd
commands = hsubparser
   ( command "ls"      (info (ListCmd    <$> listCmds)    (progDesc "List ECS resources"))
  <> command "cluster" (info (ClusterCmd <$> clusterCmds) (progDesc "Perform cluster related operations"))
  <> command "service" (info (ServiceCmd <$> serviceCmds) (progDesc "Perform service related operations"))
  -- <> command "task"    (info (TaskCmd    <$> grootTaskCli)    (progDesc "Manage ECS tasks"))
    )

grootOpts :: Parser GrootOpts
grootOpts = ( GrootOpts
          <$> credsOpt
          <*> optional regionOpt
          <*> commands
          ) <**> versionOpt

-- | Generates the AWS Env from the command line options
loadEnv :: GrootOpts -> IO Env
loadEnv opts = do
  configFile       <- defaultConfigFile
  (creds, profile) <- buildCreds $ opts ^. grootCreds
  env              <- newEnv creds
  assignRegion (findRegion configFile profile) env
    where buildCreds :: CredentialsOpt -> IO (Credentials, Maybe Text)
          buildCreds (ProfileOpt profile file) = do
            profileName <- return $ maybe defaultSectionName id profile
            credsFile   <- maybe defaultCredsFile return file
            return (FromFile profileName credsFile, Just profileName)
          buildCreds (KeysOpt accessKey secretKey) =
            return (FromKeys accessKey secretKey, Nothing)

          findRegion :: FilePath -> Maybe Text -> MaybeT IO Region
          findRegion confFile profile = regionFromOpts <|> regionFromCfg
            where regionFromOpts :: MaybeT IO Region
                  regionFromOpts = MaybeT . return $ opts ^. grootRegion

                  regionFromCfg :: MaybeT IO Region
                  regionFromCfg = MaybeT $ do
                    reg <- runExceptT $ regionFromConfig confFile profile
                    case reg of
                      Left err -> do
                        putWarn $ T.pack err
                        return Nothing
                      Right r -> return (Just r)

          assignRegion :: MaybeT IO Region -> Env -> IO Env
          assignRegion r env = do
            maybeRegion <- runMaybeT r
            return $ maybe id (\x -> envRegion .~ x) maybeRegion env

-- Groot Error handlers

handleClusterNotFound :: ClusterNotFound -> IO ()
handleClusterNotFound (ClusterNotFound' clusterRef) =
  putError $ "Could not find cluster " <> (styled yellowStyle $ toText clusterRef)

handleInvalidClusterStatus :: InvalidClusterStatus -> IO ()
handleInvalidClusterStatus (InvalidClusterStatus' clusterRef currentSt desiredSt) =
  putError $ "Can't operate on cluster " <> (styled yellowStyle $ toText clusterRef)
    <> " because it is " <> (styled redStyle $ toText currentSt)
    <> maybe "." (\x -> ", it should be " <> (styled blueStyle $ toText x) <> " to continue.") desiredSt

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

handleFailedServiceDeployment :: FailedServiceDeployment -> IO ()
handleFailedServiceDeployment (FailedServiceDeployment' serviceRef clusterRef) =
  putError $ "Service " <> (styled yellowStyle $ toText serviceRef) <> " in cluster "
    <> (styled yellowStyle $ toText clusterRef)
    <> " failed to stabilize during deployment."

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
  , handler _FailedServiceDeployment handleFailedServiceDeployment
  ]

-- | Runs a Groot command with the given AWS environment
evalCmd :: GrootCmd -> GrootM IO ()
evalCmd (ClusterCmd opts) = runClusterCmd opts
evalCmd (ListCmd opts)    = runListCmd opts
evalCmd (ServiceCmd opts) = runServiceCmd opts
--evalCmd (TaskCmd opts)    = runGrootTask opts

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
