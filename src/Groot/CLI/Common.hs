module Groot.CLI.Common where

import qualified Data.Attoparsec.Text as A
import           Data.Semigroup       ((<>))
import           Data.String
import           Options.Applicative

import           Groot.Data           (ClusterRef (..), TaskFamily (..))

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . fromString)

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
