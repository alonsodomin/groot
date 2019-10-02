{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Groot.Console
Description : Code abstrations for Groot to operate with the terminal/console
Copyright   : A. Alonso Dominguez (c) 2017, http://github.com/alonsodomin
License     : Apache 2.0
Maintainer  : A. Alonso Dominguez <alonso.domin (λ) google>
Stability   : experimental
Portability : portable

This is a module providing some abstractions to interact with the user via the
terminal's console in a rich maner.
-}
module Groot.Console
     ( Severity (..)
     , MonadConsole (..)
     -- User Prompts
     , askUserYN
     , askUserToContinue
     -- User messages
     , putInfo
     , putWarn
     , putError
     , putSuccess
     , putDebug
     ) where

import           Control.Monad                   (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Identity    (IdentityT)
import           Control.Monad.Trans.Maybe       (MaybeT)
import           Data.Semigroup
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           System.IO

import           Groot.Internal.Data.Text.Styled as ST
import           Groot.Internal.Display

errorText, warnText, infoText, successText, debugText :: StyledText
errorText   = styled redStyle    "FAIL"
warnText    = styled yellowStyle "WARN"
infoText    = styled blueStyle   "INFO"
successText = styled greenStyle  "DONE"
debugText   = styled cyanStyle   "DBUG"

-- |Severity level of the different output messages
data Severity = Error | Warn | Info | Success | Debug
  deriving (Eq, Show, Enum, Bounded, Ord)

-- |Definition of operations availbale when interacting with the user via the
-- terminal console
class Monad m => MonadConsole m where
  -- |Puts a message with a given severity in the console output
  putMessage :: Display a => Severity -> a -> m ()
  -- |Prompts the user for input
  askUser :: Display a => a -> m (Maybe Text)

instance (Monad m, MonadIO m) => MonadConsole m where
  putMessage sev txt = do
    display $ levelStr <> (ST.singleton ' ')
    displayLn txt
      where levelStr = case sev of
              Error   -> errorText
              Warn    -> warnText
              Info    -> infoText
              Success -> successText
              Debug   -> debugText

  askUser prompt = do
    answer <- liftIO $ do
      display prompt
      hFlush stdout
      T.getLine
    return $ if answer == ""
      then Nothing
      else Just answer

instance MonadConsole m => MonadConsole (IdentityT m) where
  putMessage sev txt = lift $ putMessage sev txt
  askUser = lift . askUser
instance MonadConsole m => MonadConsole (MaybeT m) where
  putMessage sev txt = lift $ putMessage sev txt
  askUser = lift . askUser

-- |Prompts the user for a 'yes' or 'no' answer
askUserYN :: MonadConsole m => Bool -> Text -> m Bool
askUserYN def msg = do
  answer <- askUser $ T.append msg defStr
  return $ handleAnswer answer
  where defStr = T.concat [ " [", (if def then "Yn" else "yN"), "] "]

        parseAnswer s =
          let s' = T.toLower s
          in (s' == "y") || (s' == "yes")

        handleAnswer Nothing  = def
        handleAnswer (Just s) = parseAnswer s

-- |Prompts the user for a 'yes' or 'no' answer. The given continuation
-- action will be performed in case she answers 'yes' or a synonym.
askUserToContinue :: MonadConsole m => Text -> m () -> m ()
askUserToContinue msg cont = do
  shouldContinue <- askUserYN False msg
  when shouldContinue cont

-- |Outputs an 'INFO' message
putInfo :: (MonadConsole m, Display a) => a -> m ()
putInfo = putMessage Info

-- |Outputs an 'WARN' message
putWarn :: (MonadConsole m, Display a) => a -> m ()
putWarn = putMessage Warn

-- |Outputs an 'ERROR' message
putError :: (MonadConsole m, Display a) => a -> m ()
putError = putMessage Error

-- |Outputs an 'SUCCESS' message
putSuccess :: (MonadConsole m, Display a) => a -> m ()
putSuccess = putMessage Success

-- |Outputs an 'DEBUG' message
putDebug :: (MonadConsole m, Display a) => a -> m ()
putDebug = putMessage Debug
