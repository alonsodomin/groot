{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Groot.Session where

import           Control.Lens
import           Control.Monad.Morph
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Network.AWS

import           Groot.Internal.AWS
import           Groot.Internal.Data.Text
import           Groot.Types

data SessionAuth = SessionAuth
  { _saRoleName    :: RoleArn
  , _saMfaDevice   :: MFADeviceArn
  , _saMfaCode     :: Maybe AuthToken
  , _saSessionName :: Maybe Text
  } deriving Eq

makeLenses ''SessionAuth

startSession :: SessionAuth -> Env -> IO Env
startSession cfg env = do
  newAuth <- authWithMfa
  return $ env & envAuth .~ newAuth

  where
    authWithMfa :: IO Auth
    authWithMfa = Auth <$> do
      let sessionName = maybe "groot" id $ cfg ^. saSessionName
      let mfaDevice = cfg ^. saMfaDevice
      let roleName = cfg ^. saRoleName

      mfaCode <- case (cfg ^. saMfaCode) of
        Just token -> pure token
        Nothing -> do
          putStr "MFA Token: "
          token <- getLine
          return . AuthToken $ T.pack token

      runResourceT $ do
        maybeAuth <- (runAWS env) . runMaybeT $ assumeRole mfaDevice mfaCode roleName sessionName
        case maybeAuth of
          Just authEnv -> pure $ authEnv
          Nothing      -> fail "Could not retrieve a valid session token."
