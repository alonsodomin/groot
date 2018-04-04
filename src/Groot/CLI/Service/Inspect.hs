module Groot.CLI.Service.Inspect
     ( ServiceInspectOpts
     , serviceInspectOpts
     , runServiceInspect
     ) where

import Control.Lens hiding (argument)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
--import System.Locale
import           Network.AWS
import qualified Network.AWS.ECS              as ECS
import           Options.Applicative
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import           Groot.AWS
import           Groot.CLI.Common
import           Groot.Core
import           Groot.Exception
import           Groot.Types

data ServiceInspectOpts =
  ServiceInspectOpts (Maybe ClusterRef) ContainerServiceRef
  deriving (Eq, Show)

serviceNameArg :: Parser ContainerServiceRef
serviceNameArg = fromString <$> argument str (metavar "SERVICE")

serviceInspectOpts :: Parser ServiceInspectOpts
serviceInspectOpts = ServiceInspectOpts
                 <$> optional clusterOpt
                 <*> serviceNameArg

defaultIndent :: Int
defaultIndent = 3

pprintService :: ECS.ContainerService -> Doc
pprintService service = Doc.vsep [
    Doc.blue $ service ^. ECS.csServiceName . txt
  , Doc.indent defaultIndent (Doc.vsep [
        (Doc.text "status:") <+> (service ^. ECS.csStatus . txt)            
      , (Doc.text "running:") <+> (service ^. ECS.csRunningCount . int)
      , (Doc.text "desired:") <+> (service ^. ECS.csDesiredCount . int)
      , (Doc.text "pending:") <+> (service ^. ECS.csPendingCount . int)
      , (Doc.text "ARNs:")
      , Doc.indent defaultIndent (Doc.vsep [
          (Doc.text "service:") <+> (service ^. ECS.csServiceARN . txt)
        , (Doc.text "task:") <+> (service ^. ECS.csTaskDefinition . txt)
        , (Doc.text "cluster:") <+> (service ^. ECS.csClusterARN . txt)
        , (Doc.text "role:") <+> (service ^. ECS.csRoleARN . txt)
      ])
      , (Doc.text "created:") <+> (service ^. ECS.csCreatedAt . time)
    ])
  ]
  where txt :: Getter (Maybe Text) Doc
        txt = to (\x -> maybe Doc.empty (Doc.text . T.unpack) x)

        int :: Getter (Maybe Int) Doc
        int = to (\x -> maybe Doc.empty Doc.int x)

        time :: Getter (Maybe UTCTime) Doc
        time = to (\x -> maybe Doc.empty (Doc.text . formatTime defaultTimeLocale "%FT%T.%q%z") x)

runServiceInspect :: ServiceInspectOpts -> GrootM IO ()
runServiceInspect (ServiceInspectOpts clusterRef serviceRef) = do
  env <- ask
  xs <- runResourceT . runAWS env $ runMaybeT $ findService serviceRef clusterRef
  case xs of
    Nothing -> throwM $ serviceNotFound serviceRef clusterRef
    Just  s -> liftIO . Doc.putDoc $ pprintService s
