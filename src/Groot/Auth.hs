{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Groot.Auth where

import           Control.Lens
import           Control.Monad.Morph
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Network.AWS

import qualified Groot.Internal.AWS.Auth      as AWS
import           Groot.Internal.Data.Text
import           Groot.Types

data RoleAuth = RoleAuth
  { _raRoleName    :: RoleArn
  , _raMfaDevice   :: MFADeviceArn
  , _raMfaCode     :: AuthToken
  , _raSessionName :: Maybe Text
  } deriving Eq

makeLenses ''RoleAuth

authRole :: RoleAuth -> Env -> IO Env
authRole cfg env = do
  newAuth <- authWithMfa
  return $ env & envAuth .~ newAuth

  where
    authWithMfa :: IO Auth
    authWithMfa = Auth <$> do
      let sessionName = maybe "groot" id $ cfg ^. raSessionName
      runResourceT $ do
        maybeAuth <- (runAWS env) . runMaybeT $ AWS.assumeRole (cfg ^. raMfaDevice) (cfg ^. raMfaCode) (cfg ^. raRoleName) sessionName

        case maybeAuth of
          Just authEnv -> pure $ authEnv
          Nothing      -> fail "Could not authenticate"
