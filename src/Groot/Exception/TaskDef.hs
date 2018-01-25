{-# LANGUAGE LambdaCase #-}

module Groot.Exception.TaskDef where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch    hiding (Handler)
import           Data.Typeable

import           Groot.Types

data TaskDefException =
    TaskDefNotFound TaskDefNotFound
  | FailedToRegisterTaskDef FailedToRegisterTaskDef
  deriving (Eq, Show, Typeable)

instance Exception TaskDefException

data TaskDefNotFound = TaskDefNotFound' TaskDefRef
  deriving (Eq, Show, Typeable)

instance Exception TaskDefNotFound

taskDefNotFound :: TaskDefRef -> SomeException
taskDefNotFound = toException . TaskDefNotFound . TaskDefNotFound'

data FailedToRegisterTaskDef = FailedToRegisterTaskDef' TaskDefRef
  deriving (Eq, Show, Typeable)

instance Exception FailedToRegisterTaskDef

failedToRegisterTaskDef :: TaskDefRef -> SomeException
failedToRegisterTaskDef = toException . FailedToRegisterTaskDef . FailedToRegisterTaskDef'

class AsTaskDefException t where
  _TaskDefException :: Prism' t TaskDefException
  {-# MINIMAL _TaskDefException #-}

  _TaskDefNotFound :: Prism' t TaskDefNotFound
  _TaskDefNotFound = _TaskDefException . _TaskDefNotFound

  _FailedToRegisterTaskDef :: Prism' t FailedToRegisterTaskDef
  _FailedToRegisterTaskDef = _TaskDefException . _FailedToRegisterTaskDef

instance AsTaskDefException SomeException where
  _TaskDefException = exception

instance AsTaskDefException TaskDefException where
  _TaskDefException = id

  _TaskDefNotFound = prism TaskDefNotFound $ \case
    TaskDefNotFound e -> Right e
    x                 -> Left x

  _FailedToRegisterTaskDef = prism FailedToRegisterTaskDef $ \case
    FailedToRegisterTaskDef e -> Right e
    x                         -> Left x
