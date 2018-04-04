{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Network.AWS
import qualified Network.AWS.ECS              as ECS
import           Options.Applicative
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import           Groot.AWS
import Groot.Data.Text (ToText, toText)
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
    Doc.blue $ maybe Doc.empty (Doc.text . T.unpack) $ service ^. ECS.csServiceName
  , Doc.indent defaultIndent (Doc.vsep [
        service ^. ECS.csStatus . plain "status:"
      , service ^. ECS.csRunningCount . plain "running:"
      , service ^. ECS.csDesiredCount . plain "desired:"
      , service ^. ECS.csPendingCount . plain "pending:"
      , (Doc.text "ARNs:")
      , Doc.indent defaultIndent (Doc.vsep [
          service ^. ECS.csServiceARN . plain "service:"
        , service ^. ECS.csTaskDefinition . plain "task:"
        , service ^. ECS.csClusterARN . plain "cluster:"
        , service ^. ECS.csRoleARN . plain "role:"
      ])
      , (Doc.text "created:") <+> (service ^. ECS.csCreatedAt . time)
    ])
  ]
  where label :: (a -> Doc) -> Text -> Getter (Maybe a) Doc
        label f lb = to (\x -> maybe Doc.empty (\y -> (Doc.text (T.unpack lb)) <+> (f y)) x)
    
        plain :: ToText a => Text -> Getter (Maybe a) Doc
        plain = label (Doc.text . T.unpack . toText)

        time :: Getter (Maybe UTCTime) Doc
        time = to (\x -> maybe Doc.empty (Doc.text . formatTime defaultTimeLocale "%FT%T.%q%z") x)

runServiceInspect :: ServiceInspectOpts -> GrootM IO ()
runServiceInspect (ServiceInspectOpts clusterRef serviceRef) = do
  env <- ask
  xs <- runResourceT . runAWS env $ runMaybeT $ findService serviceRef clusterRef
  case xs of
    Nothing -> throwM $ serviceNotFound serviceRef clusterRef
    Just  s -> liftIO . Doc.putDoc $ pprintService s
