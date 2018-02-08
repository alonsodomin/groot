{-# LANGUAGE OverloadedStrings #-}

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

data ServiceComposeOpts = ServiceComposeOpts
  { composeFile :: Maybe FilePath
  , cluster     :: ClusterRef
  , dryRun      :: Bool
  , services    :: [Text]
  } deriving (Eq, Show)

serviceNameArg :: Parser Text
serviceNameArg = fromString <$> argument str (metavar "SERVICES")

serviceComposeOpts :: Parser ServiceComposeOpts
serviceComposeOpts = ServiceComposeOpts
                 <$> optional (strOption
                   ( long "file"
                  <> short 'f'
                  <> metavar "COMPOSE_FILE"
                  <> help "Compose file" ))
                 <*> clusterOpt
                 <*> switch
                   ( long "dryRun"
                  <> short 'r'
                  <> help "Just emulate but do not perform any changes" )
                 <*> many serviceNameArg

defaultComposeFilePath :: FilePath
defaultComposeFilePath = "./groot-compose.yml"

selectServices :: MonadThrow m => [Text] -> HashMap Text ServiceDeployment -> m [NamedServiceDeployment]
selectServices [] m = pure $ Map.toList m
selectServices xs m = traverse selectService xs
  where selectService :: MonadThrow m => Text -> m NamedServiceDeployment
        selectService serviceName =
          let dep = maybe (throwM $ undefinedService serviceName) pure $ Map.lookup serviceName m
              pairUp x = (serviceName,x)
          in pairUp <$> dep

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

runDeployServices :: ServiceCompose -> ClusterRef -> [Text] -> Bool -> GrootM IO ()
runDeployServices composeDef clusterRef serviceNames isDryRun =
  let interpret = if isDryRun then dryRunServiceCompose else awsServiceCompose
      runDeploy = do
        serviceList <- selectServices serviceNames $ composeDef ^. scServices
        hoist runResourceT $ interpret $ deployServices clusterRef serviceList
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
    Right composeDef -> runDeployServices composeDef (cluster opts) (services opts) (dryRun opts)
