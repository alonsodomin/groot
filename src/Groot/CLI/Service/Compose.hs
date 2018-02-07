{-# LANGUAGE OverloadedStrings #-}

module Groot.CLI.Service.Compose
     ( ServiceComposeOpts(..)
     , serviceComposeOpts
     , runServiceCompose
     ) where

import           Control.Exception.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Semigroup         ((<>))
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Yaml              (ParseException, decodeFileEither,
                                         prettyPrintParseException)
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Console
import           Groot.Core
import           Groot.Compose
import           Groot.Data.Text
import           Groot.Exception
import           Groot.Types

data ServiceComposeOpts = ServiceComposeOpts
  { composeFile :: FilePath
  , cluster     :: ClusterRef
  , dryRun      :: Bool
  , services    :: [Text]
  } deriving (Eq, Show)

serviceNameArg :: Parser Text
serviceNameArg = fromString <$> argument str (metavar "SERVICES")

serviceComposeOpts :: Parser ServiceComposeOpts
serviceComposeOpts = ServiceComposeOpts
                 <$> strOption
                   ( long "file"
                  <> short 'f'
                  <> metavar "COMPOSE_FILE"
                  <> help "Compose file" )
                 <*> clusterOpt
                 <*> switch
                   ( long "dryRun"
                  <> short 'r'
                  <> help "Just emulate but do not perform any changes" )
                 <*> many serviceNameArg

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

runDeployServices :: ServiceCompose -> [Text] -> ClusterRef -> GrootM IO ()
runDeployServices composeDef serviceList clusterRef =
  let deployAction = composeServices composeDef serviceList clusterRef
  in catches deployAction [
    handler _UndefinedService        handleUndefinedService
  , handler _FailedServiceDeployment handleDeploymentFailed
  ]

runServiceCompose :: ServiceComposeOpts -> GrootM IO ()
runServiceCompose opts = do
  parsed <- liftIO . decodeFileEither $ composeFile opts
  case parsed of
    Left err         -> handleParseException err $ composeFile opts
    Right composeDef -> if (not $ dryRun opts)
                        then runDeployServices composeDef (services opts) (cluster opts)
                        else liftIO $ print composeDef
