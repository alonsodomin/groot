module Groot.CLI.Compose
     ( ComposeOpts(..)
     , composeOpts
     , runCompose
     ) where

import           Data.Semigroup      ((<>))
import           Data.Yaml           (decodeFileEither,
                                      prettyPrintParseException)
import           Network.AWS
import           Options.Applicative

import           Groot.Compose

data ComposeOpts = ComposeOpts
  { composeFile :: FilePath
  } deriving (Eq, Show)

composeOpts :: Parser ComposeOpts
composeOpts = ComposeOpts
          <$> strOption
            ( long "file"
           <> short 'f'
           <> metavar "COMPOSE_FILE"
           <> help "Compose file" )

runCompose :: ComposeOpts -> Env -> IO ()
runCompose opts _ = do
  parsed <- decodeFileEither $ composeFile opts
  case parsed of
    Left err         -> putStrLn $ prettyPrintParseException err
    Right composeDef -> print (composeDef :: GrootCompose)
