module Groot.App.Service.Events
     ( ServiceEventOptions
     , serviceEventsCli
     , runServiceEvents
     ) where

import Data.Conduit
import Data.Semigroup ((<>))
import Data.String
import Network.AWS hiding (await)
import Options.Applicative

import Groot.App.Cli.Parsers (clusterOpt)
import Groot.App.Events
import Groot.Core
import Groot.Data

data ServiceEventOptions = ServiceEventOptions
  { _clusterId    :: Maybe ClusterRef
  , _follow       :: Bool
  , _serviceNames :: [ServiceRef]
  } deriving (Eq, Show)

serviceRefArg :: Parser ServiceRef
serviceRefArg = fromString <$> argument str (metavar "SERVICE_NAMES")

serviceEventsCli :: Parser ServiceEventOptions
serviceEventsCli = ServiceEventOptions
               <$> optional clusterOpt
               <*> switch
                 ( long "follow"
                 <> short 'f'
                 <> help "Follow the trail of events" )
               <*> some serviceRefArg

runServiceEvents :: ServiceEventOptions -> Env -> IO ()
runServiceEvents (ServiceEventOptions (Just clusterRef) follow serviceRefs) env =
  runConduit $ fetchEvents env (map (\x -> ServiceCoords x clusterRef) serviceRefs) follow =$ printEvents
runServiceEvents (ServiceEventOptions Nothing follow serviceRefs) env = do
  coords <- runResourceT . runAWS env $ findServiceCoords serviceRefs
  runConduit $ fetchEvents env coords follow =$ printEvents