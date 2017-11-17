module Groot.App.Cluster.Events
     ( ClusterEventOptions
     , clusterEventsCli
     , runClusterEvents
     ) where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Semigroup ((<>))
import Data.String
import Network.AWS
import qualified Network.AWS.ECS as ECS
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

clusterEventsCli :: Parser ClusterEventOptions
clusterEventsCli = ClusterEventOptions
               <$> switch
                 ( long "follow"
                <> short 'f'
                <> help "Folow the trail of events" )
               <*> many clusterRefArg

fetchEvents :: Traversable t => Env -> t ClusterRef -> Bool -> Source IO ECS.ServiceEvent
fetchEvents env clusterRefs inf =
  transPipe (runResourceT . runAWS env) $ clusterServiceEventLog clusterRefs inf

runClusterEvents :: ClusterEventOptions -> Env -> IO ()
runClusterEvents (ClusterEventOptions follow [])          env = do
  clusterRefs <- runResourceT . runAWS env . sourceToList $ fetchClusters =$= CL.mapMaybe clusterName
  runClusterEvents (ClusterEventOptions follow clusterRefs) env
runClusterEvents (ClusterEventOptions follow clusterRefs) env =
  runConduit $ fetchEvents env clusterRefs follow =$ printEventSink