module Groot.CLI.Common where

import qualified Data.Attoparsec.Text as A
import           Data.Semigroup       ((<>))
import           Data.String
import           Options.Applicative

import           Groot.Manifest       (defaultManifestFilePath)
import           Groot.Types          (ClusterRef (..),
                                       ContainerServiceRef (..),
                                       TaskFamily (..))

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
