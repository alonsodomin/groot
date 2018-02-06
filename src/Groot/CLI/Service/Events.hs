{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Groot.CLI.Service.Events
     ( ServiceEventOpts
     , serviceEventsOpt
     , runServiceEvents
     ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Foldable
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import           Data.Semigroup               ((<>))
import           Data.String
import           Data.Typeable
import           Network.AWS
import qualified Network.AWS.ECS              as ECS
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Console
import           Groot.Core
import           Groot.Core.Events
import           Groot.Types

data ServiceEventOpts = ServiceEventOpts
  { _clusterId    :: Maybe ClusterRef
  , _follow       :: Bool
  , _eventCount   :: Int
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
               <*> eventCountOpt
               <*> serviceRefArgList

fetchEvents :: (Typeable mi, MonadResource mi, MonadBaseControl IO mi, MonadCatch mi, MonadConsole mi, MonadIO mo, Foldable f)
            => f ContainerServiceCoords
            -> Bool
            -> Int
            -> GrootM mi (Source mo ECS.ServiceEvent)
fetchEvents coords inf = serviceEventLog (toList coords) inf

runServiceEvents :: ServiceEventOpts -> GrootM IO ()
runServiceEvents (ServiceEventOpts (Just clusterRef) follow lastN serviceRefs) = mapReaderT runResourceT $ do
  eventSource <- fetchEvents (fmap (\x -> ContainerServiceCoords x clusterRef) serviceRefs) follow lastN
  runConduit $ eventSource =$ printEventSink
runServiceEvents (ServiceEventOpts Nothing follow lastN serviceRefs) = mapReaderT runResourceT $ do
  env         <- ask
  coords      <- runAWS env $ findServiceCoords serviceRefs
  eventSource <- fetchEvents coords follow lastN
  runConduit $ eventSource =$ printEventSink
