module Groot.App.Cluster.Events
     ( ClusterEventOptions
     , clusterEventsCli
     , runClusterEvents
     ) where

import Data.Semigroup ((<>))
import Data.String
import Network.AWS
import Options.Applicative

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

runClusterEvents :: ClusterEventOptions -> Env -> IO ()
runClusterEvents = undefined