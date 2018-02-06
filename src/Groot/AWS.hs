{-# LANGUAGE OverloadedStrings #-}

module Groot.AWS
     (
       module Groot.AWS.Cluster
     , module Groot.AWS.Instance
     , module Groot.AWS.Service
     , module Groot.AWS.Task
     , module Groot.AWS.TaskDef
     -- Error handlers
     , handleHttpException
     , handleServiceError
     ) where

import           Control.Lens
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Network.AWS
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import           Groot.AWS.Cluster
import           Groot.AWS.Instance
import           Groot.AWS.Service
import           Groot.AWS.Task
import           Groot.AWS.TaskDef
import           Groot.Console
import           Groot.Data.Text

-- AWS Error handlers

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
