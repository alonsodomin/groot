module Groot.CLI 
     ( CredentialsOpt(..)
     , CliCmd(..)
     , CliOptions(..)
     ) where

import           Options.Applicative
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

data CliCmd =
    ClusterCmd ClusterSubCmd
  | ListCmd ListSubCmd
  | ServiceCmd ServiceSubCmd
  deriving (Eq, Show)

data CliOptions = CliOptions
  { cliCreds  :: CredentialsOpt
  , cliRegion :: Maybe Region
  , cliCmd    :: CliCmd
  } deriving Eq