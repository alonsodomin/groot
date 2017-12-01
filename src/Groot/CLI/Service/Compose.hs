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

import           Groot.Compose

data ServiceComposeOpts = ServiceComposeOpts
  { composeFile :: FilePath
  } deriving (Eq, Show)

serviceComposeOpts :: Parser ServiceComposeOpts
serviceComposeOpts = ServiceComposeOpts
                 <$> strOption
                   ( long "file"
                  <> short 'f'
                  <> metavar "COMPOSE_FILE"
                  <> help "Compose file" )

runServiceCompose :: ServiceComposeOpts -> Env -> IO ()
runServiceCompose opts _ = do
  parsed <- decodeFileEither $ composeFile opts
  case parsed of
    Left err         -> putStrLn $ prettyPrintParseException err
    Right composeDef -> print (composeDef :: GrootCompose)
