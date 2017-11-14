{-# LANGUAGE LambdaCase #-}

module Groot.Exception.Task where

import Control.Exception.Lens
import Control.Monad.Catch hiding (Handler)
import Control.Lens
import Data.Typeable
import Groot.Data

data TaskException =
    TaskNotFound TaskNotFound
  | InvalidTaskStatus InvalidTaskStatus
  | TaskStatusTransitionFailed TaskStatusTransitionFailed
  deriving (Eq, Show, Typeable)

instance Exception TaskException

data TaskNotFound =
  TaskNotFound' TaskRef (Maybe ClusterRef)
  deriving (Eq, Show, Typeable)

instance Exception TaskNotFound

taskNotFound :: TaskRef -> Maybe ClusterRef -> SomeException
taskNotFound taskRef clusterRef =
  toException . TaskNotFound $ TaskNotFound' taskRef clusterRef

data InvalidTaskStatus =
  InvalidTaskStatus' TaskRef TaskStatus (Maybe TaskStatus)
  deriving (Eq, Show, Typeable)

instance Exception InvalidTaskStatus

invalidTaskStatus :: TaskRef -> TaskStatus -> Maybe TaskStatus -> SomeException
invalidTaskStatus taskRef currentSt desiredSt =
  toException . InvalidTaskStatus $ InvalidTaskStatus' taskRef currentSt desiredSt

data TaskStatusTransitionFailed =
  TaskStatusTransitionFailed' TaskRef TaskStatus TaskStatus
  deriving (Eq, Show, Typeable)

taskStatusTransitionFailed :: TaskRef -> TaskStatus -> TaskStatus -> SomeException
taskStatusTransitionFailed taskRef currentSt desiredSt =
  toException . TaskStatusTransitionFailed $ TaskStatusTransitionFailed' taskRef currentSt desiredSt

instance Exception TaskStatusTransitionFailed

class AsTaskException t where
  _TaskException :: Prism' t TaskException
  {-# MINIMAL _TaskException #-}

  _TaskNotFound :: Prism' t TaskNotFound
  _TaskNotFound = _TaskException . _TaskNotFound

  _InvalidTaskStatus :: Prism' t InvalidTaskStatus
  _InvalidTaskStatus = _TaskException . _InvalidTaskStatus

  _TaskStatusTransitionFailed :: Prism' t TaskStatusTransitionFailed
  _TaskStatusTransitionFailed = _TaskException . _TaskStatusTransitionFailed

instance AsTaskException SomeException where
  _TaskException = exception

instance AsTaskException TaskException where
  _TaskException = id

  _TaskNotFound = prism TaskNotFound $ \case
    TaskNotFound e -> Right e
    x              -> Left x

  _InvalidTaskStatus = prism InvalidTaskStatus $ \case
    InvalidTaskStatus e -> Right e
    x                   -> Left x

  _TaskStatusTransitionFailed = prism TaskStatusTransitionFailed $ \case
    TaskStatusTransitionFailed e -> Right e
    x                            -> Left x