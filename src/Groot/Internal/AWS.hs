{-# LANGUAGE OverloadedStrings #-}

module Groot.Internal.AWS
     ( module Groot.Internal.AWS.Cluster
     , module Groot.Internal.AWS.Images
     , module Groot.Internal.AWS.Instance
     , module Groot.Internal.AWS.Service
     , module Groot.Internal.AWS.Task
     , module Groot.Internal.AWS.TaskDef
     , module Groot.Internal.AWS.AutoScaling
     -- Error handlers
     , handleHttpException
     , handleServiceError
     ) where

import           Control.Lens
import           Data.Semigroup                 ((<>))
import qualified Data.Text                      as T
import           Network.AWS
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import           Groot.Console
import           Groot.Data.Text
import           Groot.Internal.AWS.AutoScaling
import           Groot.Internal.AWS.Cluster
import           Groot.Internal.AWS.Images
import           Groot.Internal.AWS.Instance
import           Groot.Internal.AWS.Service
import           Groot.Internal.AWS.Task
import           Groot.Internal.AWS.TaskDef

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
