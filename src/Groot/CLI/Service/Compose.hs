module Groot.CLI.Service.Compose
     ( ServiceComposeOpts(..)
     , serviceComposeOpts
     , runServiceCompose
     ) where

import           Control.Monad.IO.Class
import           Data.Semigroup         ((<>))
import           Data.String
import           Data.Yaml              (decodeFileEither,
                                         prettyPrintParseException)
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Core
import           Groot.Core.Compose
import           Groot.Types

data ServiceComposeOpts = ServiceComposeOpts
  { composeFile :: FilePath
  , cluster     :: ClusterRef
  , dryRun      :: Bool
  , services    :: [ContainerServiceRef]
  } deriving (Eq, Show)

serviceNameArg :: Parser ContainerServiceRef
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

runServiceCompose :: ServiceComposeOpts -> GrootM IO ()
runServiceCompose opts = do
  parsed <- liftIO . decodeFileEither $ composeFile opts
  case parsed of
    Left err         -> liftIO . putStrLn $ prettyPrintParseException err
    Right composeDef -> if (not $ dryRun opts)
                        then composeServices composeDef (cluster opts)
                        else liftIO $ print composeDef
