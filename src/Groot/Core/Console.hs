{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Groot.Core.Console
     (
       Severity (..)
     , MonadConsole (..)
     -- User Prompts
     , askUserYN
     , askUserToContinue
     -- User messages
     , putInfo
     , putWarn
     , putError
     ) where

import           Control.Monad.IO.Class
import           Data.Semigroup
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Groot.Data.Text.Display
import           Groot.Data.Text.Styled  as ST
import           System.IO

errorText, warnText, infoText :: StyledText
errorText = styled redStyle    "ERROR"
warnText  = styled yellowStyle "WARN"
infoText  = styled blueStyle   "INFO"

data Severity = Error | Warn | Info
  deriving (Eq, Show, Enum, Bounded, Ord)

class Monad m => MonadConsole m where
  putMessage :: Display a => Severity -> a -> m ()
  askUser :: Display a => a -> m (Maybe Text)

instance MonadConsole IO where
  putMessage sev txt = do
    display $ levelStr <> (ST.singleton ' ')
    display txt
      where levelStr = case sev of
              Error -> errorText
              Warn  -> warnText
              Info  -> infoText

  askUser prompt = do
    answer <- liftIO $ do
      display prompt
      hFlush stdout
      T.getLine
    return $ if answer == ""
      then Nothing
      else Just answer

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

askUserToContinue :: MonadConsole m => Text -> m () -> m ()
askUserToContinue msg cont = do
  answer <- askUserYN False msg
  if answer then cont
  else return ()

putInfo :: (MonadConsole m, Display a) => a -> m ()
putInfo = putMessage Info

putWarn :: (MonadConsole m, Display a) => a -> m ()
putWarn = putMessage Warn

putError :: (MonadConsole m, Display a) => a -> m ()
putError = putMessage Error
