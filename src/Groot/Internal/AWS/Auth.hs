module Groot.Internal.AWS.Auth where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Text                 (Text)
import           Network.AWS
import qualified Network.AWS.STS           as STS

import           Groot.Internal.Data.Text
import           Groot.Types

mfaAuth :: MonadAWS m => Text -> Text -> MaybeT m AuthEnv
mfaAuth serialNumber code = MaybeT $ (view STS.gstrsCredentials) <$> send tokenReq
  where tokenReq =
            STS.gstSerialNumber ?~ serialNumber
          $ STS.gstTokenCode ?~ code
          $ STS.getSessionToken

assumeRole :: MonadAWS m => MFADeviceArn -> AuthToken -> RoleArn -> Text -> MaybeT m AuthEnv
assumeRole deviceArn token roleArn sessionName =
  MaybeT $ (view STS.arrsCredentials) <$> send assumeRoleReq
  where assumeRoleReq =
            STS.arTokenCode ?~ (toText token)
          $ STS.arSerialNumber ?~ (toText deviceArn)
          $ STS.assumeRole (toText roleArn) sessionName
