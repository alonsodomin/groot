{-# LANGUAGE OverloadedStrings #-}

module Groot.CLI.Service.Compose
     ( ServiceComposeOpts(..)
     , serviceComposeOpts
     , runServiceUp
     , runServiceDelete
     ) where

import           Control.Lens        hiding (argument)
import           Control.Monad.Catch
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Semigroup      ((<>))
import           Data.String
import           Data.Text           (Text)
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Compose
import           Groot.Core
import           Groot.Exception
import           Groot.Manifest
import           Groot.Types

-- Command Line

data ServiceComposeOpts = ServiceComposeOpts
  { manifestFile :: Maybe FilePath
  , cluster      :: ClusterRef
  , runFlags     :: [RunFlag]
  , serviceNames :: [Text]
  } deriving (Eq, Show)

dryRunOpt :: Parser RunFlag
dryRunOpt = flag' DryRun
          ( long "dryRun"
         <> short 't'
         <> help "Just emulate but do not perform any changes" )

unattendedOpt :: Parser RunFlag
unattendedOpt = flag' Unattended
              ( long "yes"
             <> short 'y'
             <> help "Answer 'yes' to all questions." )

runFlagsOpt :: Parser [RunFlag]
runFlagsOpt = many (dryRunOpt <|> unattendedOpt)

serviceNameArg :: Parser Text
serviceNameArg = fromString <$> argument str (metavar "SERVICES")

serviceComposeOpts :: Parser ServiceComposeOpts
serviceComposeOpts = ServiceComposeOpts
                 <$> optional manifestFileOpt
                 <*> clusterOpt
                 <*> runFlagsOpt
                 <*> many serviceNameArg

-- Main functions

selectServices :: MonadThrow m => FilePath -> [Text] -> HashMap Text ServiceDeployment -> m [NamedServiceDeployment]
selectServices _        [] m = pure $ Map.toList m
selectServices manifest xs m = traverse selectService xs
  where selectService :: MonadThrow m => Text -> m NamedServiceDeployment
        selectService serviceName =
          let dep = maybe (throwM $ undefinedService serviceName manifest) pure $ Map.lookup serviceName m
              pairUp x = (serviceName,x)
          in pairUp <$> dep

performAction :: Text -> (ServiceComposeCfg -> ServiceComposeM ()) -> ServiceComposeOpts -> GrootIO ()
performAction userMsg buildComposeAction opts = do
  let manifestFileName = maybe defaultManifestFilePath id $ manifestFile opts
  manifest      <- loadManifest manifestFileName
  serviceList   <- selectServices manifestFileName (serviceNames opts) $ manifest ^. gmServices
  cfg           <- pure $ ServiceComposeCfg manifest (cluster opts) serviceList (runFlags opts)
  composeAction <- pure $ buildComposeAction cfg
  interpretServiceComposeM userMsg composeAction cfg

doDeployServices :: ServiceComposeCfg -> ServiceComposeM ()
doDeployServices (ServiceComposeCfg _ clusterRef serviceList _) =
  deployServices clusterRef serviceList

doDeleteServices :: ServiceComposeCfg -> ServiceComposeM ()
doDeleteServices (ServiceComposeCfg _ clusterRef serviceList _) =
  deleteServices clusterRef serviceList

runServiceUp :: ServiceComposeOpts -> GrootIO ()
runServiceUp = performAction "This will start deployment of the following services:" doDeployServices

runServiceDelete :: ServiceComposeOpts -> GrootIO ()
runServiceDelete = performAction "This will delete the following services:" doDeleteServices
