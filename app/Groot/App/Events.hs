module Groot.App.Events
       ( printEvents
       ) where

import           Control.Monad.IO.Class
import           Control.Lens           hiding (argument)
import           Data.Conduit
import qualified Data.Text              as T
import           Data.Time
import           Network.AWS            hiding (await)
import qualified Network.AWS.ECS        as ECS
import           System.Console.ANSI

import Groot.Core
import Groot.Data

formatEventTime :: UTCTime -> IO String
formatEventTime time = do
  dt <- utcToLocalZonedTime time
  return $ formatTime defaultTimeLocale "%d/%m/%Y %T" dt

printEvent :: ECS.ServiceEvent -> IO ()
printEvent event = do
  eventTime <- maybe (return "") formatEventTime $ event ^. ECS.seCreatedAt
  setSGR [SetColor Foreground Dull Blue]
  putStr $ eventTime
  setSGR [Reset]
  putStr " "
  putStrLn $ maybe "" T.unpack $ event ^. ECS.seMessage

printEvents :: Sink ECS.ServiceEvent IO ()
printEvents = do
  mevent <- await
  case mevent of
    Just event -> do
      liftIO $ printEvent event
      printEvents
    Nothing -> return ()
