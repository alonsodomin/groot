module Groot.App.Events
       ( EventOptions
       , grootEventsCli
       , runGrootEvents
       ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Lens hiding (argument)
import Data.Conduit
import Data.Semigroup ((<>))
import Data.String
import qualified Data.Text as T
import Network.AWS hiding (await)
import qualified Network.AWS.ECS as ECS
import Options.Applicative
import System.Console.ANSI

import Groot.App.Cli.Parsers (clusterOpt)
import Groot.Core
import Groot.Data
import Groot.Exception

data EventOptions = EventOptions
  { _clusterId   :: Maybe ClusterRef
  , _follow      :: Bool
  , _serviceName :: ServiceRef
  } deriving (Eq, Show)

grootEventsCli :: Parser EventOptions
grootEventsCli = EventOptions
             <$> optional clusterOpt
             <*> switch
               ( long "follow"
              <> short 'f'
              <> help "Follow the trail of events" )
             <*> (fromString <$> argument str (metavar "SERVICE_NAME"))

printEvent :: ECS.ServiceEvent -> IO ()
printEvent event = do
  putStr "[ "
  setSGR [SetColor Foreground Dull Blue]
  putStr $ padL 27 $ maybe "" show $ event ^. ECS.seCreatedAt
  setSGR [Reset]
  putStr " ] - "
  putStrLn $ maybe "" T.unpack $ event ^. ECS.seMessage
  where padL :: Int -> String -> String
        padL n s
          | length s < n = s ++ replicate (n - length s) ' '
          | otherwise    = s

findServiceCoords :: MonadAWS m => ServiceRef -> m ServiceCoords
findServiceCoords serviceRef = do
  mcoords <- (serviceCoords <$> getService serviceRef Nothing)
  case mcoords of
    Just c  -> return c
    Nothing -> throwM $ serviceNotFound serviceRef Nothing

fetchEvents :: Env -> ServiceCoords -> Bool -> Source IO ECS.ServiceEvent
fetchEvents env coords inf =
  transPipe (runResourceT . runAWS env) $ serviceEventLog coords inf

printEvents :: Sink ECS.ServiceEvent IO ()
printEvents = do
  mevent <- await
  case mevent of
    Just event -> do
      liftIO $ printEvent event
      printEvents
    Nothing -> return ()

runGrootEvents :: EventOptions -> Env -> IO ()
runGrootEvents (EventOptions (Just clusterRef) follow serviceRef) env =
  runConduit $ fetchEvents env (ServiceCoords serviceRef clusterRef) follow =$ printEvents
runGrootEvents (EventOptions Nothing follow serviceRef) env = do
  coords <- runResourceT . runAWS env $ findServiceCoords serviceRef
  runConduit $ fetchEvents env coords follow =$ printEvents
