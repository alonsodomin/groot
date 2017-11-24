module Groot.App.Service.Events
     ( ServiceEventOptions
     , serviceEventsCli
     , runServiceEvents
     ) where

import Data.Conduit
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import Data.String
import Network.AWS
import qualified Network.AWS.ECS as ECS
import Options.Applicative

import Groot.App.Cli.Parsers (clusterOpt)
import Groot.Core
import Groot.Core.Events
import Groot.Data

data ServiceEventOptions = ServiceEventOptions
  { _clusterId    :: Maybe ClusterRef
  , _follow       :: Bool
  , _serviceNames :: NonEmpty ServiceRef
  } deriving (Eq, Show)

serviceRefArg :: Parser ServiceRef
serviceRefArg = fromString <$> argument str (metavar "SERVICE_NAMES")

serviceRefArgList :: Parser (NonEmpty ServiceRef)
serviceRefArgList = fmap (\x -> (head x) :| (tail x)) (some serviceRefArg)

serviceEventsCli :: Parser ServiceEventOptions
serviceEventsCli = ServiceEventOptions
               <$> optional clusterOpt
               <*> switch
                 ( long "follow"
                 <> short 'f'
                 <> help "Follow the trail of events" )
               <*> serviceRefArgList

fetchEvents :: Foldable f => Env -> f ServiceCoords -> Bool -> IO (Source IO ECS.ServiceEvent)
fetchEvents env coords inf = serviceEventLog env (toList coords) inf

runServiceEvents :: ServiceEventOptions -> Env -> IO ()
runServiceEvents (ServiceEventOptions (Just clusterRef) follow serviceRefs) env = do
  eventSource <- fetchEvents env (fmap (\x -> ServiceCoords x clusterRef) serviceRefs) follow
  runConduit $ eventSource =$ printEventSink
runServiceEvents (ServiceEventOptions Nothing follow serviceRefs) env = do
  coords      <- runResourceT . runAWS env $ findServiceCoords serviceRefs
  eventSource <- fetchEvents env coords follow
  runConduit $ eventSource =$ printEventSink