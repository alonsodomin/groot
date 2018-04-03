{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Groot.CLI.Service.Compose
     ( ServiceComposeOpts(..)
     , serviceComposeOpts
     , runServiceCompose
     ) where

import           Control.Exception.Lens
import           Control.Lens                 hiding (argument)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Trans.Resource
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.Semigroup               ((<>))
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Yaml                    (ParseException, decodeFileEither,
                                               prettyPrintParseException)
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Compose
import           Groot.Console
import           Groot.Core
import           Groot.Data.Text
import           Groot.Exception
import           Groot.Types

data RunMode = DryRun | Unattended
  deriving (Eq, Show, Enum, Ord)

_DryRun :: Prism' RunMode Bool
_DryRun = prism (const DryRun) $ \case
  DryRun -> Right True
  x      -> Left x

_Unattended :: Prism' RunMode Bool
_Unattended = prism (const Unattended) $ \case
  Unattended -> Right True
  x          -> Left x

data ServiceComposeOpts = ServiceComposeOpts
  { composeFile :: Maybe FilePath
  , cluster     :: ClusterRef
  , runMode     :: Maybe RunMode
  , services    :: [Text]
  } deriving (Eq, Show)

composeFileOpt :: Parser FilePath
composeFileOpt = strOption
               ( long "file"
              <> short 'f'
              <> metavar "COMPOSE_FILE"
              <> help "Compose file" )

dryRunOpt :: Parser RunMode
dryRunOpt = flag' DryRun
          ( long "dryRun"
         <> short 'r'
         <> help "Just emulate but do not perform any changes" )

unattendedOpt :: Parser RunMode
unattendedOpt = flag' Unattended
              ( long "yes"
             <> short 'y'
             <> help "Answer 'yes' to all questions." )

runModeOpt :: Parser RunMode
runModeOpt = dryRunOpt <|> unattendedOpt

serviceNameArg :: Parser Text
serviceNameArg = fromString <$> argument str (metavar "SERVICES")

serviceComposeOpts :: Parser ServiceComposeOpts
serviceComposeOpts = ServiceComposeOpts
                 <$> optional composeFileOpt
                 <*> clusterOpt
                 <*> optional runModeOpt
                 <*> many serviceNameArg

defaultComposeFilePath :: FilePath
defaultComposeFilePath = "./groot-compose.yml"

-- Error handlers

handleUndefinedService :: MonadConsole m => UndefinedService -> m ()
handleUndefinedService (UndefinedService' serviceName) =
  putError $ "Service" <+> (styled yellowStyle serviceName) <+> "has not been defined in compose file."

handleDeploymentFailed :: MonadConsole m => FailedServiceDeployment -> m ()
handleDeploymentFailed (FailedServiceDeployment' serviceRef clusterRef reason) =
  putError $ "Failed to deploy service" <+> (styled yellowStyle $ toText serviceRef)
    <+> "in cluster" <+> (styled yellowStyle $ toText clusterRef)
    <> (maybe "" (\x -> " because" <+> (styled yellowStyle x)) reason)

handleParseException :: MonadConsole m => ParseException -> FilePath -> m ()
handleParseException err file = do
  msg <- pure $ prettyPrintParseException err
  putError $ "Could not parse service compose file: " <> (styled blueStyle $ T.pack file) <> "\n"
    <> (styled yellowStyle $ T.pack msg)

-- Main functions

selectServices :: MonadThrow m => [Text] -> HashMap Text ServiceDeployment -> m [NamedServiceDeployment]
selectServices [] m = pure $ Map.toList m
selectServices xs m = traverse selectService xs
  where selectService :: MonadThrow m => Text -> m NamedServiceDeployment
        selectService serviceName =
          let dep = maybe (throwM $ undefinedService serviceName) pure $ Map.lookup serviceName m
              pairUp x = (serviceName,x)
          in pairUp <$> dep

runDeployServices :: ServiceCompose -> ClusterRef -> [Text] -> Maybe RunMode -> GrootM IO ()
runDeployServices composeDef clusterRef serviceNames rm =
  let shouldConfirm   = maybe True (isn't _Unattended) rm
      isDryRun        = maybe False (\x -> not $ isn't _DryRun x) rm
      confirmMsg srvs = "This will start deployment of the following services:\n" <>
        (T.intercalate "\n" $ T.append "   - " . fst <$> srvs) <> ".\nDo you want to continue? "
      interpret       = if isDryRun then dryRunServiceCompose else awsServiceCompose
      doDeploy srvs   = if shouldConfirm
        then askUserToContinue (confirmMsg srvs) (interpret $ deployServices clusterRef srvs)
        else interpret $ deployServices clusterRef srvs
      runDeploy       = do
        serviceList <- selectServices serviceNames $ composeDef ^. scServices
        hoist runResourceT $ doDeploy serviceList
  in catches runDeploy [
    handler _UndefinedService        handleUndefinedService
  , handler _FailedServiceDeployment handleDeploymentFailed
  ]

runServiceCompose :: ServiceComposeOpts -> GrootM IO ()
runServiceCompose opts = do
  givenFile <- pure $ maybe defaultComposeFilePath id $ composeFile opts
  parsed    <- liftIO $ decodeFileEither givenFile
  case parsed of
    Left err         -> handleParseException err givenFile
    Right composeDef -> runDeployServices composeDef (cluster opts) (services opts) (runMode opts)
