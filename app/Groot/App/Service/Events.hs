module Groot.App.Service.Events
     ( ServiceEventOptions
     , serviceEventsCli
     , runServiceEvents
     ) where

import Data.Conduit
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

fetchEvents :: (Traversable t) => Env -> t ServiceCoords -> Bool -> Source IO ECS.ServiceEvent
fetchEvents env coords inf =
  transPipe (runResourceT . runAWS env) $ servicesEventLog coords inf

runServiceEvents :: ServiceEventOptions -> Env -> IO ()
runServiceEvents (ServiceEventOptions (Just clusterRef) follow serviceRefs) env =
  runConduit $ fetchEvents env (fmap (\x -> ServiceCoords x clusterRef) serviceRefs) follow =$ printEventSink
runServiceEvents (ServiceEventOptions Nothing follow serviceRefs) env = do
  coords <- runResourceT . runAWS env $ findServiceCoords serviceRefs
  runConduit $ fetchEvents env coords follow =$ printEventSink