{-# LANGUAGE OverloadedStrings #-}

module Groot.CLI.Service.Compose
     ( ServiceComposeOpts(..)
     , serviceComposeOpts
     , runServiceUp
     , runServiceDelete
     ) where

import           Control.Exception.Lens
import           Control.Lens           hiding (argument)
import           Control.Monad.Catch
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as Map
import           Data.Semigroup         ((<>))
import           Data.String
import           Data.Text              (Text)
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Compose
import           Groot.Console
import           Groot.Core
import           Groot.Data.Text
import           Groot.Exception
import           Groot.Manifest
import           Groot.Types

-- Command Line

data ServiceComposeOpts = ServiceComposeOpts
  { manifestFile :: Maybe FilePath
  , cluster      :: ClusterRef
  , runMode      :: Maybe RunMode
  , serviceNames :: [Text]
  } deriving (Eq, Show)

dryRunOpt :: Parser RunMode
dryRunOpt = flag' DryRun
          ( long "dryRun"
         <> short 't'
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
                 <$> optional manifestFileOpt
                 <*> clusterOpt
                 <*> optional runModeOpt
                 <*> many serviceNameArg

-- Error handlers

handleUndefinedService :: MonadConsole m => UndefinedService -> m ()
handleUndefinedService (UndefinedService' serviceName) =
  putError $ "Service" <+> (styled yellowStyle serviceName) <+> "has not been defined in compose file."

handleDeploymentFailed :: MonadConsole m => FailedServiceDeployment -> m ()
handleDeploymentFailed (FailedServiceDeployment' serviceRef clusterRef reason) =
  putError $ "Failed to deploy service" <+> (styled yellowStyle $ toText serviceRef)
    <+> "in cluster" <+> (styled yellowStyle $ toText clusterRef)
    <> (maybe "" (\x -> " because" <+> (styled yellowStyle x)) reason)

-- Main functions

selectServices :: MonadThrow m => [Text] -> HashMap Text ServiceDeployment -> m [NamedServiceDeployment]
selectServices [] m = pure $ Map.toList m
selectServices xs m = traverse selectService xs
  where selectService :: MonadThrow m => Text -> m NamedServiceDeployment
        selectService serviceName =
          let dep = maybe (throwM $ undefinedService serviceName) pure $ Map.lookup serviceName m
              pairUp x = (serviceName,x)
          in pairUp <$> dep

performAction :: Text -> (ServiceComposeCfg -> ServiceComposeM ()) -> ServiceComposeOpts -> GrootM IO ()
performAction userMsg buildComposeAction opts = do
  manifest      <- loadManifest $ manifestFile opts
  serviceList   <- selectServices (serviceNames opts) $ manifest ^. gmServices
  cfg           <- pure $ ServiceComposeCfg manifest (cluster opts) serviceList (runMode opts)
  composeAction <- pure $ buildComposeAction cfg
  catches (interpretServiceComposeM userMsg composeAction cfg) [
      handler _UndefinedService        handleUndefinedService
    , handler _FailedServiceDeployment handleDeploymentFailed
    ]

doDeployServices :: ServiceComposeCfg -> ServiceComposeM ()
doDeployServices (ServiceComposeCfg _ clusterRef serviceList _) =
  deployServices clusterRef serviceList

doDeleteServices :: ServiceComposeCfg -> ServiceComposeM ()
doDeleteServices (ServiceComposeCfg _ clusterRef serviceList _) =
  deleteServices clusterRef serviceList

runServiceUp :: ServiceComposeOpts -> GrootM IO ()
runServiceUp = performAction "This will start deployment of the following services:" doDeployServices

runServiceDelete :: ServiceComposeOpts -> GrootM IO ()
runServiceDelete = performAction "This will delete the following services:" doDeleteServices
