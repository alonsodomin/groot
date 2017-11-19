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
import qualified Data.Attoparsec.Text as A
import Data.Semigroup ((<>))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.AWS (
    AccessKey(..)
  , SecretKey(..)
  , Region
  )
import Network.AWS.Data.Text

import Groot.Data (
    ClusterRef(..)
  , TaskFamily(..)
  )

data AwsCredentials =
    AwsProfile (Maybe Text) (Maybe FilePath)
  | AwsKeys AccessKey SecretKey
  deriving Eq

credsOpt :: Parser AwsCredentials
credsOpt =
  let profile = T.pack <$> strOption
                ( long "profile"
               <> short 'p'
               <> metavar "AWS_PROFILE"
               <> help "AWS Profile" )
      accessKey = (AccessKey . encodeUtf8 . T.pack) <$> strOption
                  ( long "accessKey"
                 <> metavar "ACCESS_KEY"
                 <> help "AWS Access Key" )
      secretKey = (SecretKey . encodeUtf8 . T.pack) <$> strOption
                  ( long "secretKey"
                 <> metavar "SECRET_KEY"
                 <> help "AWS Secret Key" )
      file = strOption
             ( long "creds"
            <> metavar "CRENDENTIALS_FILE"
            <> help "AWS Credentials config file" )
  in let fromProfile = AwsProfile <$> (optional profile) <*> (optional file)
         fromKeys    = AwsKeys <$> accessKey <*> secretKey
     in fromKeys <|> fromProfile

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . T.pack)

regionOpt :: Parser Region
regionOpt = option (attoReadM parser)
          ( long "region"
         <> short 'r'
         <> metavar "AWS_REGION"
         <> help "AWS Region identifier" )

clusterOpt :: Parser ClusterRef
clusterOpt = fromString <$> strOption
           ( long "cluster"
          <> short 'c'
          <> metavar "CLUSTER_REF"
          <> help "ECS Cluster reference (name or ARN)" )

taskFamilyOpt :: Parser TaskFamily
taskFamilyOpt = fromString <$> strOption
              ( long "family"
             <> metavar "TASK_FAMILY"
             <> help "ECS Task Family" )
