module Groot.App.Cli.Parsers
     ( 
     -- Data types
       AwsCredentials(..)
     -- Parsers
     , credsOpt
     , clusterOpt
     , regionOpt
     , taskFamilyOpt
     ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.AWS (
    AccessKey(..)
  , SecretKey(..)
  , Region
  )

import Groot.Data (
    ClusterRef(..)
  , mkClusterRef
  , TaskFamily(..)
  )

data AwsCredentials =
    AwsProfile (Maybe Text) (Maybe FilePath)
  | AwsKeys AccessKey SecretKey
  deriving Eq

credsOpt :: Parser AwsCredentials
credsOpt =
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

regionOpt :: Parser Region
regionOpt = option auto
          ( long "region"
         <> short 'r'
         <> metavar "AWS_REGION"
         <> help "AWS Region" )

clusterOpt :: Parser ClusterRef
clusterOpt = mkClusterRef <$> strOption
           ( long "cluster"
          <> short 'c'
          <> metavar "CLUSTER_REF"
          <> help "ECS Cluster reference (name or ARN)" )

taskFamilyOpt :: Parser TaskFamily
taskFamilyOpt = TaskFamily . pack <$> strOption
              ( long "family"
             <> metavar "TASK_FAMILY"
             <> help "ECS Task Family" )
