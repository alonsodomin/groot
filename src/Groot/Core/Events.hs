module Groot.Core.Events
       ( printEvent
       , printEventSink
       ) where

import           Control.Monad.IO.Class
import           Control.Lens           hiding (argument)
import           Data.Conduit
import qualified Data.Text              as T
import           Data.Time
import qualified Network.AWS.ECS        as ECS
import           System.Console.ANSI

formatEventTime :: MonadIO m => UTCTime -> m String
formatEventTime time = do
  dt <- liftIO $ utcToLocalZonedTime time
  return $ formatTime defaultTimeLocale "%d/%m/%Y %T" dt

printEvent :: MonadIO m => ECS.ServiceEvent -> m ()
printEvent event = do
  eventTime <- maybe (return "") formatEventTime $ event ^. ECS.seCreatedAt
  liftIO $ do
    setSGR [SetColor Foreground Dull Blue]
    putStr $ eventTime
    setSGR [Reset]
    putStrLn $ ' ':(maybe "" T.unpack $ event ^. ECS.seMessage)

printEventSink :: MonadIO m => Sink ECS.ServiceEvent m ()
printEventSink = do
  mevent <- await
  case mevent of
    Just event -> do
      printEvent event
      printEventSink
    Nothing -> return ()
