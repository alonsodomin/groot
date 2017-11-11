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
import Groot.App.Cli.Parsers (clusterOpt)
import Groot.Core
import Groot.Data
import Groot.Exception
import Network.AWS hiding (await)
import qualified Network.AWS.ECS as ECS
import Text.PrettyPrint.Boxes (printBox, (<+>))
import qualified Text.PrettyPrint.Boxes as B
import Options.Applicative

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

layoutEvent :: ECS.ServiceEvent -> B.Box
layoutEvent event =
  (B.text "[") <+>
  (B.text . padL 27 $ maybe "" show $ event ^. ECS.seCreatedAt) <+>
  (B.text "]") <+>
  (B.text "-") <+>
  (B.text $ maybe "" T.unpack $ event ^. ECS.seMessage)
  where padL :: Int -> String -> String
        padL n str
          | length str < n = str ++ replicate (n - length str) ' '
          | otherwise      = str

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
      liftIO $ printBox . layoutEvent $ event
      printEvents
    Nothing -> return ()

runGrootEvents :: EventOptions -> Env -> IO ()
runGrootEvents (EventOptions (Just clusterRef) follow serviceRef) env =
  runConduit $ fetchEvents env (ServiceCoords serviceRef clusterRef) follow =$ printEvents
runGrootEvents (EventOptions Nothing follow serviceRef) env = do
  coords <- runResourceT . runAWS env $ findServiceCoords serviceRef
  runConduit $ fetchEvents env coords follow =$ printEvents
