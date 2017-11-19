{-# LANGUAGE OverloadedStrings #-}

module Groot.App 
     ( groot
     ) where

import           Control.Applicative
import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8      as BS
import           Data.List                  (intercalate)
import           Data.Semigroup
import qualified Data.Text                  as T
import           Network.AWS
import           Network.AWS.Data.Text
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status
import           System.Console.ANSI

import Groot.App.Cli
import Groot.App.Compose
import Groot.App.Cluster
import Groot.App.Service
import Groot.App.List
import Groot.App.Task
import Groot.Config
import Groot.Core.Console
import Groot.Data
import Groot.Exception

loadEnv :: CliOptions -> IO Env
loadEnv opts = do
  configFile       <- defaultConfigFile
  (creds, profile) <- buildCreds $ opts ^. cliAwsCreds
  env              <- newEnv creds
  assignRegion (findRegion configFile profile) env
    where buildCreds :: AwsCredentials -> IO (Credentials, Maybe Text)
          buildCreds (AwsProfile profile file) = do
            profileName <- return $ maybe defaultSectionName id profile
            credsFile   <- maybe defaultCredsFile return file
            return (FromFile profileName credsFile, Just profileName)
          buildCreds (AwsKeys accessKey secretKey) =
            return (FromKeys accessKey secretKey, Nothing)

          findRegion :: FilePath -> Maybe Text -> MaybeT IO Region
          findRegion confFile profile = regionFromOpts <|> regionFromCfg
            where regionFromOpts = MaybeT . return $ opts ^? cliAwsRegion
                  regionFromCfg  = MaybeT $ do
                    reg <- runExceptT $ regionFromConfig confFile profile
                    case reg of
                      Left err -> do
                        printWarn err
                        return Nothing
                      Right r -> return (Just r)

          assignRegion :: MaybeT IO Region -> Env -> IO Env
          assignRegion r env = do
            maybeRegion <- runMaybeT r
            return $ maybe id (\x -> envRegion .~ x) maybeRegion env

grootCmd :: Cmd -> Env -> IO ()
grootCmd (ComposeCmd opts) = runGrootCompose opts
grootCmd (ClusterCmd opts) = runGrootCluster opts
grootCmd (ListCmd opts)    = runGrootList opts
grootCmd (ServiceCmd opts) = runGrootService opts
grootCmd (TaskCmd opts)    = runGrootTask opts

-- AWS Error handlers

handleHttpException :: HttpException -> IO ()
handleHttpException (InvalidUrlException url reason) =
  printError $ "Url " <> url <> " is invalid due to: " <> reason
handleHttpException (HttpExceptionRequest req content) =
  printError $ "Could not communicate with '" <> (BS.unpack . host $ req) <> "'."

handleServiceError :: ServiceError -> IO ()
handleServiceError err =
  let servName  = T.unpack . toText $ err ^. serviceAbbrev
      statusMsg = BS.unpack . statusMessage $ err ^. serviceStatus
      message   = maybe "" (T.unpack . toText) $ err ^. serviceMessage
  in do
    putError
    putStr " "
    setSGR [SetColor Foreground Dull Yellow]
    putStr $ concat [servName, " ", statusMsg]
    setSGR [Reset]
    putStrLn $ ' ':message

-- Groot Error handlers

handleClusterNotFound :: ClusterNotFound -> IO ()
handleClusterNotFound (ClusterNotFound' (ClusterRef ref)) =
  printError $ "Could not find cluster '" <> (T.unpack ref) <> "'"

handleServiceNotFound :: ServiceNotFound -> IO ()
handleServiceNotFound (ServiceNotFound' serviceRef clusterRef) =
  printError $ "Could not find service '" <> (T.unpack . toText $ serviceRef) <> "'" <>
    maybe "" (\x -> " in cluster " <> (T.unpack . toText $ x)) clusterRef

handleAmbiguousServiceName :: AmbiguousServiceName -> IO ()
handleAmbiguousServiceName (AmbiguousServiceName' serviceRef clusters) =
  let stringifyClusters = "\n - " <> (intercalate "\n - " $ map (T.unpack . toText) clusters)
  in printError $ "Service name '" <> (T.unpack . toText $ serviceRef) 
     <> "' is ambiguous. It was found in the following clusters:" <> stringifyClusters

handleInactiveService :: InactiveService -> IO ()
handleInactiveService (InactiveService' serviceRef clusterRef) =
  printError $ "Service '" <> (T.unpack . toText $ serviceRef) <> "' in cluster '"
    <> (T.unpack . toText $ clusterRef) <> "' is not active."

handleTaskNotFound :: TaskNotFound -> IO ()
handleTaskNotFound (TaskNotFound' taskRef clusterRef) =
  printError $ "Could not find task '" <> (T.unpack . toText $ taskRef) <> "'" <>
    maybe "" (\x -> " in cluster " <> (T.unpack . toText $ x)) clusterRef

-- Main Program execution

handleExceptions :: IO () -> IO ()
handleExceptions action = catches action [
    handler _TransportError       handleHttpException
  , handler _ServiceError         handleServiceError
  , handler _ClusterNotFound      handleClusterNotFound
  , handler _ServiceNotFound      handleServiceNotFound
  , handler _AmbiguousServiceName handleAmbiguousServiceName
  , handler _InactiveService      handleInactiveService
  , handler _TaskNotFound         handleTaskNotFound
  ]

groot :: CliOptions -> IO ()
groot opts = handleExceptions $ loadEnv opts >>= grootCmd (opts ^. cliCmd)