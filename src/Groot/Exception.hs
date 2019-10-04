{-# LANGUAGE OverloadedStrings #-}

module Groot.Exception
     ( module Groot.Exception.Cluster
     , module Groot.Exception.Instance
     , module Groot.Exception.Task
     , module Groot.Exception.TaskDef
     , module Groot.Exception.Service
     , module Groot.Exception.Manifest
     -- Handlers
     , handleExceptions
     , handleExceptions_
     , handleExceptionsAndExit
     , handleHttpException
     , handleServiceError
     ) where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Text                 as T
import           Data.Typeable
import           Network.AWS
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status
import           System.Exit

import           Groot.Console
import           Groot.Exception.Cluster
import           Groot.Exception.Instance
import           Groot.Exception.Manifest
import           Groot.Exception.Service
import           Groot.Exception.Task
import           Groot.Exception.TaskDef
import           Groot.Internal.Data.Text

-- Groot Error Handlers

handleClusterNotFound :: MonadConsole m => ClusterNotFound -> m ()
handleClusterNotFound (ClusterNotFound' clusterRef) =
  putError $ "Could not find cluster " <> (styled yellowStyle $ toText clusterRef)

handleInvalidClusterStatus :: MonadConsole m => InvalidClusterStatus -> m ()
handleInvalidClusterStatus (InvalidClusterStatus' clusterRef currentSt desiredSt) =
  putError $ "Can't operate on cluster " <> (styled yellowStyle $ toText clusterRef)
    <> " because it is " <> (styled redStyle $ toText currentSt)
    <> maybe "." (\x -> ", it should be " <> (styled greenStyle $ toText x) <> " to continue.") desiredSt

handleServiceNotFound :: MonadConsole m => ServiceNotFound -> m ()
handleServiceNotFound (ServiceNotFound' serviceRef clusterRef) =
  putError $ "Could not find service " <> (styled yellowStyle $ toText serviceRef) <>
    maybe "" (\x -> " in cluster " <> (styled yellowStyle $ toText x)) clusterRef

handleAmbiguousServiceName :: MonadConsole m => AmbiguousServiceName -> m ()
handleAmbiguousServiceName (AmbiguousServiceName' serviceRef clusters) =
  let stringifyClusters = styleless $ "\n - " <> (T.intercalate "\n - " $ toText <$> clusters)
  in putError $ "Service name " <> (styled yellowStyle $ toText serviceRef)
       <> " is ambiguous. It was found in the following clusters:" <> stringifyClusters

handleInactiveService :: MonadConsole m => InactiveService -> m ()
handleInactiveService (InactiveService' serviceRef clusterRef) =
  putError $ "Service " <> (styled yellowStyle $ toText serviceRef) <> " in cluster "
    <> (styled yellowStyle $ toText $ clusterRef)
    <> " is not active."

handleTaskNotFound :: MonadConsole m => TaskNotFound -> m ()
handleTaskNotFound (TaskNotFound' taskRef clusterRef) =
  putError $ "Could not find task " <> (styled yellowStyle $ toText taskRef) <>
    maybe "" (\x -> " in cluster " <> (styled yellowStyle $ toText x)) clusterRef

handleManifestParseError :: MonadConsole m => ManifestParseError -> m ()
handleManifestParseError (ManifestParseError' file reason) =
     putError $ "Could not parse manifest file: " <> (styled blueStyle $ T.pack file) <> "\n"
     <> (styled yellowStyle reason)

-- AWS Error Handlers

handleHttpException :: MonadConsole m => HttpException -> m ()
handleHttpException (InvalidUrlException url reason) =
  putError $ "Url " <> (styled yellowStyle $ toText url) <> " is invalid due to: " <> (styled redStyle $ toText reason)
handleHttpException (HttpExceptionRequest req _) =
  putError $ "Could not communicate with " <> (styled yellowStyle $ toText . host $ req) <> "."

handleServiceError :: MonadConsole m => ServiceError -> m ()
handleServiceError err =
  let servName  = toText $ err ^. serviceAbbrev
      statusMsg = toText . statusMessage $ err ^. serviceStatus
      message   = maybe "" toText $ err ^. serviceMessage
      styledSt  = styled yellowStyle (T.concat [servName, " ", statusMsg])
  in putError $ styledSt <+> (styleless message)

-- Main Handler

allHandlers' :: (MonadCatch m, MonadConsole m, Typeable m) => m a -> [Handler m a]
allHandlers' cont =
  [ handler _TransportError          $ \ex -> handleHttpException        ex >> cont
  , handler _ServiceError            $ \ex -> handleServiceError         ex >> cont
  , handler _ClusterNotFound         $ \ex -> handleClusterNotFound      ex >> cont
  , handler _InvalidClusterStatus    $ \ex -> handleInvalidClusterStatus ex >> cont
  , handler _ServiceNotFound         $ \ex -> handleServiceNotFound      ex >> cont
  , handler _AmbiguousServiceName    $ \ex -> handleAmbiguousServiceName ex >> cont
  , handler _InactiveService         $ \ex -> handleInactiveService      ex >> cont
  , handler _TaskNotFound            $ \ex -> handleTaskNotFound         ex >> cont
  , handler _ManifestParseError      $ \ex -> handleManifestParseError   ex >> cont
  ]

allHandlers :: (MonadCatch m, MonadConsole m, Typeable m) => [Handler m ()]
allHandlers = allHandlers' $ return ()

handleExceptions :: (MonadCatch m, MonadConsole m, Typeable m) => a -> m a -> m a
handleExceptions def action =
  catches action $ map (fmap (\_ -> def)) allHandlers

handleExceptions_ :: (MonadCatch m, MonadConsole m, Typeable m) => m () -> m ()
handleExceptions_ action = catches action allHandlers

handleExceptionsAndExit :: (MonadIO m, MonadCatch m, MonadConsole m, Typeable m) => m a -> m a
handleExceptionsAndExit action = catches action $ allHandlers' (liftIO exitFailure)
