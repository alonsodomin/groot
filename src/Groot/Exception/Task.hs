{-# LANGUAGE LambdaCase #-}

module Groot.Exception.Task where

import Control.Exception.Lens
import Control.Monad.Catch hiding (Handler)
import Control.Lens
import Data.Typeable
import Groot.Data

data TaskException =
  TaskNotFound TaskNotFound
  deriving (Eq, Show, Typeable)

instance Exception TaskException

data TaskNotFound =
  TaskNotFound' TaskRef (Maybe ClusterRef)
  deriving (Eq, Show, Typeable)

instance Exception TaskNotFound

taskNotFound :: TaskRef -> Maybe ClusterRef -> SomeException
taskNotFound taskRef clusterRef =
  toException . TaskNotFound $ TaskNotFound' taskRef clusterRef

class AsTaskException t where
  _TaskException :: Prism' t TaskException

  _TaskNotFound :: Prism' t TaskNotFound
  _TaskNotFound = _TaskException . _TaskNotFound

instance AsTaskException SomeException where
  _TaskException = exception

instance AsTaskException TaskException where
  _TaskException = id

  _TaskNotFound = prism TaskNotFound $ \case
    TaskNotFound e -> Right e
    x              -> Left x