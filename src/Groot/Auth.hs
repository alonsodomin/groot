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

data RoleAuth = RoleAuth
  { _raRoleName  :: Text
  , _raMfaDevice :: Text
  , _raMfaCode   :: Text
  } deriving (Eq, Show)

makeLenses ''RoleAuth

authRole :: RoleAuth -> Env -> IO Env
authRole cfg env = do
  newAuth <- authWithMfa
  return $ env & envAuth .~ newAuth

  where
    authWithMfa :: IO Auth
    authWithMfa = Auth <$> authToken

    authToken :: IO AuthEnv
    authToken = runResourceT $ do
      maybeAuth <- runMaybeT $ do
        sessionAuth <- hoist (runAWS env) $ AWS.mfaAuth (cfg ^. raMfaDevice) (cfg ^. raMfaCode)
        let sessionEnv = env & envAuth .~ (Auth sessionAuth)
        token <- MaybeT . pure $ sessionAuth ^. sessionToken
        hoist (runAWS sessionEnv) $ AWS.assumeRole (cfg ^. raRoleName) (toText token)

      case maybeAuth of
        Just authEnv -> pure $ authEnv
        Nothing      -> fail "Could not authenticate"
