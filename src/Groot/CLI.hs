module Groot.CLI 
     ( CredentialsOpt(..)
     , GrootCmd(..)
     , GrootOpts(..)
     , runGroot
     ) where

import           Control.Monad.Trans.Maybe      
import qualified Data.Attoparsec.Text as A
import           Data.Semigroup ((<>))
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Network.AWS (
                               AccessKey(..)
                             , SecretKey(..)
                             , Region
                             )
import           Options.Applicative
import           Paths_groot (version)

import Groot.CLI.Common
import Groot.CLI.Cluster
import Groot.CLI.List
import Groot.CLI.Service
import Groot.Data (
    ClusterRef(..)
  , TaskFamily(..)
  )
import Groot.Data.Text hiding (Parser, option)

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
  { cliCreds  :: CredentialsOpt
  , cliRegion :: Maybe Region
  , cliCmd    :: GrootCmd
  } deriving Eq

commands :: Parser GrootCmd
commands = hsubparser
   ( command "ls"      (info (ListCmd    <$> listCmds)    (progDesc "List ECS resources"))
  <> command "cluster" (info (ClusterCmd <$> clusterCmds) (progDesc "Perform cluster related operations"))
  <> command "service" (info (ServiceCmd <$> serviceCmds) (progDesc "Perform service related operations"))
  -- <> command "compose" (info (ComposeCmd <$> grootComposeCli) (progDesc "Handle Groot compose files"))
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
  (creds, profile) <- buildCreds $ opts ^. cliAwsCreds
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
            where regionFromOpts = MaybeT . return $ opts ^? cliAwsRegion
                  regionFromCfg  = MaybeT $ do
                    reg <- runExceptT $ regionFromConfig confFile profile
                    case reg of
                      Left err -> do
                        printWarn err
                        return Nothing
                      Right r -> return (Just r)

          assignRegion :: MaybeT IO Region -> Env -> IO Env
          assignRegion r env = do
            maybeRegion <- runMaybeT r
            return $ maybe id (\x -> envRegion .~ x) maybeRegion env

runCmd :: GrootCmd -> Env -> IO ()
--runCmd (ComposeCmd opts) = runGrootCompose opts
runCmd (ClusterCmd opts) = runClusterCmd opts
runCmd (ListCmd opts)    = runListCmd opts
runCmd (ServiceCmd opts) = runServiceCmd opts
--runCmd (TaskCmd opts)    = runGrootTask opts

runGroot :: GrootOpts -> IO ()
runGroot opts =
  prog =<< (execParser cli)
  where prog = do
          env <- loadEnv opts
          runCmd (cliCmd opts) env
        
        cli = info (grootOpts <**> helper)
          ( fullDesc
          <> progDesc "Utility to manage ECS Clusters"
          <> header "groot" )