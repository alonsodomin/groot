module Groot.CLI.Common where

import qualified Data.Attoparsec.Text     as A
import           Data.Semigroup           ((<>))
import           Data.String
import           Network.AWS              (Region)
import           Options.Applicative

import           Groot.Internal.Data.Text
import           Groot.Manifest           (defaultManifestFilePath)
import           Groot.Types              (ClusterRef (..),
                                           ContainerServiceRef (..),
                                           MFACredentials, TaskFamily (..),
                                           mfaCredentials)

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . fromString)

clusterOpt :: Parser ClusterRef
clusterOpt = fromString <$> strOption
            ( long "cluster"
          <> short 'c'
          <> metavar "CLUSTER_REF"
          <> help "ECS Cluster reference (name or ARN)" )

containerServiceOpt :: Parser ContainerServiceRef
containerServiceOpt = fromString <$> strOption
                    ( long "service"
                   <> short 's'
                   <> metavar "SERVICE_REF"
                   <> help "ECS Service reference (name or ARN)" )

taskFamilyOpt :: Parser TaskFamily
taskFamilyOpt = fromString <$> strOption
              ( long "family"
              <> metavar "TASK_FAMILY"
              <> help "ECS Task Family" )

eventCountOpt :: Parser Int
eventCountOpt = option auto
              ( long "number"
             <> short 'n'
             <> help "Number of events"
             <> showDefault
             <> value 25 )

manifestFileOpt :: Parser FilePath
manifestFileOpt = strOption
                ( long "file"
               <> short 'f'
               <> metavar "MANIFEST_FILE"
               <> help ("Groot manifest file (default: " ++ defaultManifestFilePath ++ ")" ))

mfaCredentialsOpt :: Parser MFACredentials
mfaCredentialsOpt = mfaCredentials <$> mfaDevice  <*> mfaAuthToken
  where mfaDevice = option (attoReadM parser)
                  ( long "mfa-device"
                  <> metavar "MFA_DEVICE"
                  <> help "The serial number (or ARN) of the MFA device" )

        mfaAuthToken = fromString <$> strOption
                      ( long "mfa-token"
                      <> metavar "MFA_TOKEN"
                      <> help "The temporary auth token from the MFA device" )

regionOpt :: Parser Region
regionOpt = option (attoReadM parser)
          ( long "region"
          <> short 'r'
          <> metavar "REGION"
          <> help "AWS Region identifier" )
