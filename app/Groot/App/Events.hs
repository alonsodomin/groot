module Groot.App.Events
       ( EventOptions
       , grootEventsCli
       , runGrootEvents
       , fetchEvents
       , printEvents
       ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Lens hiding (argument)
import Data.Conduit
import Data.Semigroup ((<>))
import Data.String
import qualified Data.Text as T
import Data.Time
import Network.AWS hiding (await)
import qualified Network.AWS.ECS as ECS
import Options.Applicative
import System.Console.ANSI

import Groot.App.Cli.Parsers (clusterOpt)
import Groot.Core
import Groot.Data
import Groot.Exception

data EventOptions = EventOptions
  { _clusterId    :: Maybe ClusterRef
  , _follow       :: Bool
  , _serviceNames :: [ServiceRef]
  } deriving (Eq, Show)

serviceRefArg :: Parser ServiceRef
serviceRefArg = fromString <$> argument str (metavar "SERVICE_NAMES")

grootEventsCli :: Parser EventOptions
grootEventsCli = EventOptions
             <$> optional clusterOpt
             <*> switch
               ( long "follow"
              <> short 'f'
              <> help "Follow the trail of events" )
             <*> some serviceRefArg

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

fetchEvents :: Env -> [ServiceCoords] -> Bool -> Source IO ECS.ServiceEvent
fetchEvents env coords inf =
  transPipe (runResourceT . runAWS env) $ servicesEventLog coords inf

printEvents :: Sink ECS.ServiceEvent IO ()
printEvents = do
  mevent <- await
  case mevent of
    Just event -> do
      liftIO $ printEvent event
      printEvents
    Nothing -> return ()

runGrootEvents :: EventOptions -> Env -> IO ()
runGrootEvents (EventOptions (Just clusterRef) follow serviceRefs) env =
  runConduit $ fetchEvents env (map (\x -> ServiceCoords x clusterRef) serviceRefs) follow =$ printEvents
runGrootEvents (EventOptions Nothing follow serviceRefs) env = do
  coords <- runResourceT . runAWS env $ findServiceCoords serviceRefs
  runConduit $ fetchEvents env coords follow =$ printEvents
