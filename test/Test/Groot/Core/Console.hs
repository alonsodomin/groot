{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Groot.Core.Compose where

import           Control.Monad.Free
import           Data.Text               (Text)

import           Groot.Core.Console
import           Groot.Data.Text.Display

data Console a =
    PutMessage Severity Text a
  | AskUser Text (Maybe Text -> a)
  deriving Functor

type ConsoleM = Free Console

instance MonadConsole ConsoleM where
  putMessage sev txt = PutMessage sev (toText txt) ()
  askUser txt = AskUser (toText txt) id
