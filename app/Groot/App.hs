{-# LANGUAGE OverloadedStrings #-}

module Groot.App 
     ( groot
     ) where

import Control.Applicative
import Control.Exception.Lens
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import qualified Data.Text as T
import Network.AWS
import Network.AWS.Data.Text
import Network.HTTP.Types.Status

import Groot.App.Cli
import Groot.App.Config
import Groot.App.Compose
import Groot.App.Events
import Groot.App.List
import Groot.Data
import Groot.Exception

loadEnv :: CliOptions -> IO Env
loadEnv opts = do
  configFile       <- defaultConfigFile
  (creds, profile) <- buildCreds $ awsCreds opts
  env              <- newEnv creds
  assignRegion (findRegion configFile profile) env
    where buildCreds :: AwsCredentials -> IO (Credentials, Maybe Text)
          buildCreds (AwsProfile profile file) = do
            profileName <- return $ maybe defaultProfileName id profile
            credsFile   <- maybe defaultCredsFile return file
            return (FromFile profileName credsFile, Just profileName)
          buildCreds (AwsKeys accessKey secretKey) =
            return (FromKeys accessKey secretKey, Nothing)

          findRegion :: FilePath -> Maybe Text -> MaybeT IO Region
          findRegion confFile profile = regionFromOpts <|> regionFromConfig confFile profile
            where regionFromOpts = MaybeT . return $ awsRegion opts

          assignRegion :: MaybeT IO Region -> Env -> IO Env
          assignRegion r env = do
            maybeRegion <- runMaybeT r
            return $ maybe id (\x -> envRegion .~ x) maybeRegion env

grootCmd :: Cmd -> Env -> IO ()
grootCmd (ComposeCmd opts) = runGrootCompose opts
grootCmd (ListCmd opts)    = runGrootList opts
grootCmd (EventsCmd opts)  = runGrootEvents opts

-- AWS Error handlers

handleServiceError :: ServiceError -> IO ()
handleServiceError err =
  let servName  = T.unpack . toText $ err ^. serviceAbbrev
      statusMsg = BS.unpack . statusMessage $ err ^. serviceStatus
      message   = maybe "" (T.unpack . toText) $ err ^. serviceMessage
  in putStrLn $ servName ++ " Error (" ++ statusMsg ++ "): " ++ message

-- Groot Error handlers

handleClusterNotFound :: ClusterNotFound -> IO ()
handleClusterNotFound (ClusterNotFound' (ClusterRef ref)) =
  putStrLn $ "Could not find cluster '" ++ (T.unpack ref) ++ "'"

handleServiceNotFound :: ServiceNotFound -> IO ()
handleServiceNotFound (ServiceNotFound' serviceRef clusterRef) =
  putStrLn $ "Could not find service '" ++ (T.unpack . toText $ serviceRef) ++ "'" ++
    maybe "" (\x -> " in cluster " ++ (T.unpack . toText $ x)) clusterRef

handleAmbiguousServiceName :: AmbiguousServiceName -> IO ()
handleAmbiguousServiceName (AmbiguousServiceName' serviceRef clusters) =
  let stringifyClusters = intercalate "\n - " $ map (T.unpack . toText) clusters
  in putStrLn $ "Service name '" ++ (T.unpack . toText $ serviceRef) 
     ++ "' is ambiguous. It was found in the following clusters:\n" ++ stringifyClusters

-- Main Program execution

handleExceptions :: IO () -> IO ()
handleExceptions action = catches action [
    handler _ServiceError handleServiceError
  , handler _ClusterNotFound handleClusterNotFound
  , handler _ServiceNotFound handleServiceNotFound
  , handler _AmbiguousServiceName handleAmbiguousServiceName
  ]

groot :: CliOptions -> IO ()
groot opts = handleExceptions $ loadEnv opts >>= grootCmd (cmd opts)