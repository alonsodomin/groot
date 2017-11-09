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
import Text.PrettyPrint.Boxes (printBox, (<+>))
import qualified Text.PrettyPrint.Boxes as B
import Options.Applicative

data EventOptions = EventOptions
  { _clusterId   :: ClusterId
  , _follow      :: Bool
  , _serviceName :: Text
  } deriving (Eq, Show)

grootEventsCli :: Parser EventOptions
grootEventsCli = EventOptions
             <$> clusterIdParser
             <*> switch
               ( long "follow"
              <> short 'f'
              <> help "Follow the trail of events" )
             <*> (pack <$> argument str (metavar "SERVICE_NAME"))

layoutEvent :: ECS.ServiceEvent -> B.Box
layoutEvent event =
  (B.text "[") <+>
  (B.text . padL 27 $ maybe "" show $ event ^. ECS.seCreatedAt) <+>
  (B.text "]") <+>
  (B.text "-") <+>
  (B.text $ maybe "" unpack $ event ^. ECS.seMessage)
  where padL :: Int -> String -> String
        padL n str
          | length str < n = str ++ replicate (n - length str) ' '
          | otherwise      = str

fetchEvents :: Env -> Text -> ClusterId -> Maybe UTCTime -> IO ([ECS.ServiceEvent], Maybe UTCTime)
fetchEvents env name cid lastEvtTime =
  runAction env latestEvents eventsAndLastTime
  where latestEvents = serviceEvents name cid lastEvtTime

        eventsAndLastTime :: [ECS.ServiceEvent] -> ([ECS.ServiceEvent], Maybe UTCTime)
        eventsAndLastTime events = (events, listToMaybe events >>= view ECS.seCreatedAt)

printEvents :: [ECS.ServiceEvent] -> IO ()
printEvents events =
  forM_ (reverse events) printEvent
  where printEvent :: ECS.ServiceEvent -> IO ()
        printEvent = printBox . layoutEvent

runGrootEvents :: EventOptions -> Env -> IO ()
runGrootEvents (EventOptions clusterId follow serviceName) env =
  loop Nothing
  where loop :: Maybe UTCTime -> IO ()
        loop lastEvtTime = do
          (evts, nextTime) <- fetchEvents env serviceName clusterId lastEvtTime
          printEvents evts
          if follow then do
            threadDelay 1000000
            loop $ nextTime <|> lastEvtTime
          else return ()
