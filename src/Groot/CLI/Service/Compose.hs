module Groot.CLI.Service.Compose
     ( ServiceComposeOpts(..)
     , serviceComposeOpts
     , runServiceCompose
     ) where

import           Data.Semigroup      ((<>))
import           Data.Yaml           (decodeFileEither,
                                      prettyPrintParseException)
import           Network.AWS
import           Options.Applicative

import           Groot.CLI.Common
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

runServiceCompose :: ServiceComposeOpts -> Env -> IO ()
runServiceCompose opts env = do
  parsed <- decodeFileEither $ composeFile opts
  case parsed of
    Left err         -> putStrLn $ prettyPrintParseException err
    Right composeDef -> do
      runResourceT . runAWS env $ composeServices composeDef (cluster opts)
      print (composeDef :: GrootCompose)
