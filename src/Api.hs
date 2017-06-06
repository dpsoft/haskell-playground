{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import qualified Data.Proxy  as Proxy
import qualified Data.Text   as Text
import           Servant.API as Servant
import           Model

{- |
    A value-level proxy for the type-level API.
-}
api :: Proxy.Proxy API
api = Proxy.Proxy

type API
    = ListConfigurations
    :<|> NewConfiguration
    :<|> ShowConfiguration
    :<|> UpdateConfiguration
    :<|> DeleteConfiguration

{- |
    Get all of the configurations.
-}
type ListConfigurations = "configurations" :> Servant.Get '[Servant.JSON] [Configuration]

{- |
    Create a new configuration.
-}
type NewConfiguration = "configurations"
  :> Servant.ReqBody '[Servant.JSON] Configuration
  :> Servant.Post '[Servant.JSON] Configuration

{- |
    Try to get a particular configuration.
-}
type ShowConfiguration = "configuration"
  :> Servant.Capture "id" Int
  :> Servant.Get '[Servant.JSON] Configuration

{- |
    Update an existing widget.
-}
type UpdateConfiguration = "configuration"
  :> Servant.Capture "id" Int
  :> Servant.ReqBody '[Servant.JSON] Configuration
  :> Servant.Put '[Servant.JSON] Configuration

{- |
    Update an existing widget.
-}
type DeleteConfiguration = "configuration"
  :> Servant.Capture "id" Int
  :> Servant.Delete '[Servant.JSON] Configuration
