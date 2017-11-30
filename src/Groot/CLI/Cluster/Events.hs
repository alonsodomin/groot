module Groot.CLI.Cluster.Events
     ( ClusterEventOptions
     , clusterEventsOpt
     , runClusterEvents
     ) where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Semigroup ((<>))
import Data.String
import Network.AWS
import Options.Applicative

import Groot.Core
import Groot.Core.Events
import Groot.Data

data ClusterEventOptions = ClusterEventOptions
  { _follow       :: Bool
  , _clusterNames :: [ClusterRef]
  } deriving (Eq, Show)

clusterRefArg :: Parser ClusterRef
clusterRefArg = fromString <$> argument str (metavar "CLUSTER_NAMES")

clusterEventsOpt :: Parser ClusterEventOptions
clusterEventsOpt = ClusterEventOptions
                <$> switch
                  ( long "follow"
                <> short 'f'
                <> help "Folow the trail of events" )
                <*> many clusterRefArg

runClusterEvents :: ClusterEventOptions -> Env -> IO ()
runClusterEvents (ClusterEventOptions follow []) env = do
  clusterRefs <- runResourceT . runAWS env . sourceToList $ fetchClusters =$= CL.mapMaybe clusterName
  runClusterEvents (ClusterEventOptions follow clusterRefs) env
runClusterEvents (ClusterEventOptions follow clusterRefs) env = runResourceT $ do
  eventSource <- clusterServiceEventLog env clusterRefs follow
  runConduit $ eventSource =$ printEventSink