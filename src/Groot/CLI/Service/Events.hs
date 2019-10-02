{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Groot.CLI.Service.Events
     ( ServiceEventOpts
     , serviceEventsOpt
     , runServiceEvents
     ) where

import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Foldable
import           Data.List.NonEmpty           (NonEmpty ((:|)), intersperse)
import           Data.Semigroup               ((<>))
import           Data.String
import           Network.AWS
import qualified Network.AWS.ECS              as ECS
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Console
import           Groot.Core
import           Groot.Core.Events
import           Groot.Internal.Data.Text
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

fetchEvents :: (MonadIO mi, Foldable f)
            => f ContainerServiceCoords
            -> Bool
            -> Int
            -> GrootIOResource (ConduitT () ECS.ServiceEvent mi ())
fetchEvents coords inf = serviceEventLog (toList coords) inf

runServiceEvents :: ServiceEventOpts -> GrootIO ()
runServiceEvents (ServiceEventOpts (Just clusterRef) follow lastN serviceRefs) = useResource $ do
  eventSource <- fetchEvents (fmap (\x -> ContainerServiceCoords x clusterRef) serviceRefs) follow lastN
  runConduit $ eventSource .| printEventSink
runServiceEvents (ServiceEventOpts Nothing follow lastN serviceRefs) = useResource $ do
  env         <- ask
  putInfo $ "Scanning clusters for services: " <> (fold . intersperse (styleless ", ") $ (styled yellowStyle . toText) <$> serviceRefs)
  coords      <- runAWS env $ findServiceCoords serviceRefs
  eventSource <- fetchEvents coords follow lastN
  runConduit $ eventSource .| printEventSink
