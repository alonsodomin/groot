module Groot.App.Cluster.Events
     ( ClusterEventOptions
     , clusterEventsCli
     , runClusterEvents
     ) where

import Data.Conduit
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
               <*> some clusterRefArg

fetchEvents :: Env -> [ClusterRef] -> Bool -> Source IO ECS.ServiceEvent
fetchEvents env clusterRefs inf =
  transPipe (runResourceT . runAWS env) $ clusterServiceEventLog clusterRefs inf

runClusterEvents :: ClusterEventOptions -> Env -> IO ()
runClusterEvents (ClusterEventOptions follow clusterRefs) env =
  runConduit $ fetchEvents env clusterRefs follow =$ printEventSink