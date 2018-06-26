module Groot.CLI.Cluster.Events
     ( ClusterEventOptions
     , clusterEventsOpt
     , runClusterEvents
     ) where

import           Data.Conduit
import qualified Data.Conduit.List   as CL
import           Data.Semigroup      ((<>))
import           Data.String
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Core
import           Groot.Core.Events
import           Groot.Types

data ClusterEventOptions = ClusterEventOptions
  { _follow       :: Bool
  , _eventCount   :: Int
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
                <*> eventCountOpt
                <*> many clusterRefArg

runClusterEvents :: ClusterEventOptions -> GrootIO ()
runClusterEvents (ClusterEventOptions follow lastN []) = do
  clusterRefs <- runGrootResource . awsResource . sourceToList $ fetchClusters .| CL.mapMaybe clusterName
  runClusterEvents (ClusterEventOptions follow lastN clusterRefs)
runClusterEvents (ClusterEventOptions follow lastN clusterRefs) = runGrootResource $ do
  eventSource <- clusterServiceEventLog clusterRefs follow lastN
  runConduit $ eventSource .| printEventSink
