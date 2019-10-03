module Groot.Internal.AWS.Auth where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Text                 (Text)
import           Network.AWS
import qualified Network.AWS.STS           as STS

mfaAuth :: MonadAWS m => Text -> Text -> MaybeT m AuthEnv
mfaAuth serialNumber code = MaybeT $ (view STS.gstrsCredentials) <$> send tokenReq
  where tokenReq =
            STS.gstSerialNumber ?~ serialNumber
          $ STS.gstTokenCode ?~ code
          $ STS.getSessionToken

assumeRole :: MonadAWS m => Text -> Text -> MaybeT m AuthEnv
assumeRole roleArn sessionName =
  MaybeT $ (view STS.arrsCredentials) <$> send (STS.assumeRole roleArn sessionName)
