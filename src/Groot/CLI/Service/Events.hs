{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.CLI.Service.Events
     ( ServiceEventOpts
     , serviceEventsOpt
     , runServiceEvents
     ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Foldable
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import           Data.Semigroup               ((<>))
import           Data.String
import           Network.AWS
import qualified Network.AWS.ECS              as ECS
import           Options.Applicative

import           Groot.CLI.Common             (clusterOpt)
import           Groot.Core
import           Groot.Core.Events
import           Groot.Types

data ServiceEventOpts = ServiceEventOpts
  { _clusterId    :: Maybe ClusterRef
  , _follow       :: Bool
  , _serviceNames :: NonEmpty ContainerServiceRef
  } deriving (Eq, Show)

serviceRefArg :: Parser ContainerServiceRef
serviceRefArg = fromString <$> argument str (metavar "SERVICE_NAMES")

serviceRefArgList :: Parser (NonEmpty ContainerServiceRef)
serviceRefArgList = fmap (\x -> (head x) :| (tail x)) (some serviceRefArg)

serviceEventsOpt :: Parser ServiceEventOpts
serviceEventsOpt = ServiceEventOpts
               <$> optional clusterOpt
               <*> switch
                 ( long "follow"
                 <> short 'f'
                 <> help "Follow the trail of events" )
               <*> serviceRefArgList

fetchEvents :: (MonadResource mi, MonadBaseControl IO mi, MonadIO mo, Foldable f)
            => Env
            -> f ContainerServiceCoords
            -> Bool
            -> mi (Source mo ECS.ServiceEvent)
fetchEvents env coords inf = serviceEventLog env (toList coords) inf

runServiceEvents :: ServiceEventOpts -> Env -> IO ()
runServiceEvents (ServiceEventOpts (Just clusterRef) follow serviceRefs) env = runResourceT $ do
  eventSource <- fetchEvents env (fmap (\x -> ContainerServiceCoords x clusterRef) serviceRefs) follow
  runConduit $ eventSource =$ printEventSink
runServiceEvents (ServiceEventOpts Nothing follow serviceRefs) env = runResourceT $ do
  coords      <- runAWS env $ findServiceCoords serviceRefs
  eventSource <- fetchEvents env coords follow
  runConduit $ eventSource =$ printEventSink
