module Groot.CLI.Auth where

import           Data.String
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Network.AWS
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Internal.Data.Text
import           Groot.Session
import           Groot.Types

data CredentialsOpt =
    CredsContainer
  | CredsDiscover
  | CredsExplicit AccessKey SecretKey (Maybe SessionToken)
  | CredsFile Text FilePath
  | CredsIamProfile Text
  | CredsEnvVars Text Text (Maybe Text) (Maybe Text)
  deriving Eq

credsOpt :: Parser Credentials
credsOpt =
      (asCreds <$> containerCreds)
  <|> (asCreds <$> explicitCreds)
  <|> (asCreds <$> iamProfileCreds)
  <|> (asCreds <$> fileCreds)
  <|> (asCreds <$> envCreds)
  <|> (pure Discover)

  where
    -- Mapper from opts to AWS Credentials
    asCreds :: CredentialsOpt -> Credentials
    asCreds CredsContainer = FromContainer
    asCreds (CredsExplicit accessKey secretKey (Just session)) =
      FromSession accessKey secretKey session
    asCreds (CredsExplicit accessKey secretKey Nothing) =
      FromKeys accessKey secretKey
    asCreds (CredsFile profile file) = FromFile profile file
    asCreds (CredsIamProfile profile) = FromProfile profile
    asCreds (CredsEnvVars accessKey secretKey session region) =
      FromEnv accessKey secretKey session region

    -- Constructors
    containerCreds  = flag' CredsContainer (long "from-container" <> help "Use ECS Container credentials")
    explicitCreds   = CredsExplicit <$>  accessKey <*> secretKey <*> (optional $ (SessionToken . fromString) <$> sessionToken)
    iamProfileCreds = CredsIamProfile <$> iamProfile
    fileCreds       = CredsFile <$> profile <*> file
    envCreds        = CredsEnvVars <$> keyEnv <*> secretEnv <*> (optional $ T.pack <$> sessionToken) <*> (optional regionEnv)

    -- Individual items
    profile = T.pack <$> strOption
            ( long "profile"
          <> short 'p'
          <> metavar "CREDENTIALS_PROFILE"
          <> help "AWS Credentials Profile" )

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

    regionEnv = T.pack <$> strOption
        ( long "region-var"
      <> metavar "REGION_VAR"
      <> help "AWS Region Environment Variable" )

sessionAuthOpt :: Parser SessionAuth
sessionAuthOpt = SessionAuth <$> roleName <*> (optional mfaCredentialsOpt) <*> sessionName
  where
    roleName :: Parser RoleArn
    roleName = option (attoReadM parser)
      ( long "role"
      <> metavar "ROLE_ARN"
      <> help "Role ARN to be assumed" )

    sessionName = optional $ T.pack <$> strOption
      (long "session-name"
      <> metavar "SESSION_NAME"
      <> help "Temporary session identifier" )
