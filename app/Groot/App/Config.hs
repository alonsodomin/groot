{-# LANGUAGE OverloadedStrings #-}

module Groot.App.Config
       (
       -- Default config values
         defaultProfileName
       , defaultConfigFile
       , defaultCredsFile
       -- Utilities
       , regionFromConfig
       ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Ini
import Data.Text (Text)
import qualified Data.Text as T
import Network.AWS (Region(..))
import Network.AWS.Data.Text
import System.Directory

defaultProfileName :: Text
defaultProfileName = "default"

defaultConfigFile :: IO FilePath
defaultConfigFile = (++ "/.aws/config") <$> getHomeDirectory

defaultCredsFile :: IO FilePath
defaultCredsFile = (++ "/.aws/credentials") <$> getHomeDirectory

warnUser :: ExceptT String IO a -> MaybeT IO a
warnUser result = MaybeT $ do
  err <- runExceptT result
  case err of
    Left msg -> do
      putStrLn $ "Warning: " ++ msg
      return Nothing
    Right a ->
      return $ Just a

sectionNameFromProfile :: Text -> Text
sectionNameFromProfile s
  | s == defaultProfileName = s
  | otherwise               = T.append "profile " s

regionFromConfig :: FilePath -> Maybe Text -> MaybeT IO Region
regionFromConfig filename profile = warnUser $ do
  ini <- ExceptT $ readIniFile filename
  rid <- ExceptT . return $ lookupValue profileSection "region" ini
  liftIO $ parseRegion rid
    where profileSection :: Text
          profileSection = maybe defaultProfileName sectionNameFromProfile profile

          parseRegion :: Text -> IO Region
          parseRegion rid = do
            parsed <- return $ fromText rid
            case parsed of
              Left err -> fail $ "Could not parse region from file "
                              ++ filename ++ " - " ++ err
              Right r  -> return r
