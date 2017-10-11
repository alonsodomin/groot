module Groot.App.Events
       ( EventOptions
       , grootEventsCli
       , runGrootEvents
       ) where

import Control.Concurrent (threadDelay)
import Control.Lens hiding (argument)
import Data.Foldable (forM_)
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Time.Clock
import Groot.App.Cli.Parsers (clusterIdParser)
import Groot.Core
import Groot.Data
import Network.AWS
import qualified Network.AWS.ECS as ECS
import Options.Applicative

data EventOptions = EventOptions
  { _clusterId   :: ClusterId
  , _serviceName :: Text
  , _follow      :: Bool
  } deriving (Eq, Show)

grootEventsCli :: Parser EventOptions
grootEventsCli = EventOptions
             <$> clusterIdParser
             <*> (pack <$> argument str (metavar "SERVICE_NAME"))
             <*> switch
               ( long "follow"
              <> short 'f'
              <> help "Follow the trail of events" )

eventAsString :: ECS.ServiceEvent -> String
eventAsString event = eventDT ++ " - " ++ eventMsg
  where eventDT  = "[" ++ (maybe "" show $ event ^. ECS.seCreatedAt) ++ "]"
        eventMsg = maybe "" unpack $ event ^. ECS.seMessage

fetchEvents :: Env -> Text -> ClusterId -> Maybe UTCTime -> IO ([ECS.ServiceEvent], Maybe UTCTime)
fetchEvents env name cid lastEvtTime =
  runAction env latestEvents eventsAndLastTime
  where latestEvents = serviceEvents name cid lastEvtTime
    
        eventsAndLastTime :: [ECS.ServiceEvent] -> ([ECS.ServiceEvent], Maybe UTCTime)
        eventsAndLastTime events = (events, listToMaybe events >>= view ECS.seCreatedAt)

printEvents :: [ECS.ServiceEvent] -> IO ()
printEvents events =
  forM_ (reverse $ eventAsString <$> events) putStrLn

runGrootEvents :: EventOptions -> Env -> IO ()
runGrootEvents (EventOptions clusterId serviceName follow) env =
  loop Nothing
  where loop :: Maybe UTCTime -> IO ()
        loop lastEvtTime = do
          (evts, nextTime) <- fetchEvents env serviceName clusterId lastEvtTime
          printEvents evts
          if follow then do
            threadDelay 1000000
            loop (nextTime <|> lastEvtTime)
          else return ()