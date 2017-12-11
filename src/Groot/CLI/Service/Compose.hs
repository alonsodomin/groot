module Groot.CLI.Service.Compose
     ( ServiceComposeOpts(..)
     , serviceComposeOpts
     , runServiceCompose
     ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Semigroup         ((<>))
import           Data.Yaml              (decodeFileEither,
                                         prettyPrintParseException)
import           Network.AWS
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Core
import           Groot.Core.Compose
import           Groot.Types

data ServiceComposeOpts = ServiceComposeOpts
  { composeFile :: FilePath
  , cluster     :: ClusterRef
  } deriving (Eq, Show)

serviceComposeOpts :: Parser ServiceComposeOpts
serviceComposeOpts = ServiceComposeOpts
                 <$> strOption
                   ( long "file"
                  <> short 'f'
                  <> metavar "COMPOSE_FILE"
                  <> help "Compose file" )
                 <*> clusterOpt

runServiceCompose :: ServiceComposeOpts -> GrootM IO ()
runServiceCompose opts = do
  parsed <- liftIO . decodeFileEither $ composeFile opts
  case parsed of
    Left err         -> liftIO . putStrLn $ prettyPrintParseException err
    Right composeDef -> do
      composeServices composeDef (cluster opts)
      liftIO $ print (composeDef :: GrootCompose)
