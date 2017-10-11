module Groot.App.Compose
     (
       ComposeOptions (..)
     , grootComposeCli
     , runGrootCompose
     ) where

import Data.Yaml
import Data.Semigroup ((<>))
import Network.AWS (Env)
import qualified Options.Applicative as OA

import Groot.Compose

data ComposeOptions = ComposeOptions
  { composeFile :: String }
  deriving (Eq, Show)

grootComposeCli :: OA.Parser ComposeOptions
grootComposeCli = ComposeOptions
         <$> OA.strOption
             ( OA.long "composeFile"
            <> OA.short 'f'
            <> OA.metavar "COMPOSE_FILE"
            <> OA.help "Compose definition file" )

parseGrootComposeFile :: String -> IO (Either ParseException GrootCompose)
parseGrootComposeFile file = decodeFileEither file :: IO (Either ParseException GrootCompose)

runGrootCompose :: ComposeOptions -> Env -> IO ()
runGrootCompose (ComposeOptions file) _ =
  parseGrootComposeFile file >>= describeCompo
  where describeCompo :: Either ParseException GrootCompose -> IO ()
        describeCompo res =
          case res of
            Left err -> putStrLn $ prettyPrintParseException err
            Right d -> putStrLn $ show d
