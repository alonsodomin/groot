module Groot.App.Cli.Parsers
     ( 
     -- Data types
       AwsCredentials(..)
     -- Parsers
     , credsParser
     , clusterIdParser
     , regionParser
     , taskFamilyParser
     ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.AWS (
    AccessKey(..)
  , SecretKey(..)
  , SessionToken(..)
  , Credentials(..)
  , Region
  )

import Groot.Data (ClusterId(..), TaskFamily(..))

data AwsCredentials =
    AwsProfile (Maybe Text) (Maybe FilePath)
  | AwsKeys AccessKey SecretKey
  deriving Eq

credsParser :: Parser AwsCredentials
credsParser =
  let profile = pack <$> strOption
                ( long "profile"
               <> short 'p'
               <> metavar "AWS_PROFILE"
               <> help "AWS Profile" )
      accessKey = (AccessKey . encodeUtf8 . pack) <$> strOption
                  ( long "accessKey"
                 <> metavar "ACCESS_KEY"
                 <> help "AWS Access Key" )
      secretKey = (SecretKey . encodeUtf8 . pack) <$> strOption
                  ( long "secretKey"
                 <> metavar "SECRET_KEY"
                 <> help "AWS Secret Key" )
      file = strOption
             ( long "env"
            <> metavar "ENV"
            <> help "AWS Environment config file" )
  in let fromProfile = AwsProfile <$> (optional profile) <*> (optional file)
         fromKeys    = AwsKeys <$> accessKey <*> secretKey
     in fromKeys <|> fromProfile

regionParser :: Parser Region
regionParser = option auto
             ( long "region"
            <> short 'r'
            <> metavar "AWS_REGION"
            <> help "AWS Region" )

clusterIdParser :: Parser ClusterId
clusterIdParser = ClusterId . pack <$> strOption
                ( long "clusterId"
               <> metavar "CLUSTER_ID"
               <> help "ECS Cluster identifier" )

taskFamilyParser :: Parser TaskFamily
taskFamilyParser = TaskFamily . pack <$> strOption
                 ( long "family"
                <> metavar "TASK_FAMILY"
                <> help "ECS Task Family" )
