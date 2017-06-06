{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
  The server that implements the API.
-}
module Server where

import Servant
import Api
import Model

import qualified Control.Monad.IO.Class as IO
import qualified Database as DB
import qualified Control.Monad.Trans.Either as Either


{- |
  This is the actual implementation of the server.
-}

server :: Servant.Server Api.API
server =  listConfigurations
      :<|> newConfiguration
      :<|> showConfiguration
      :<|> updateConfiguration
      :<|> deleteConfiguration

{- |
    Get all of the configurations. See 'API.ListConfigurations'.
-}
listConfigurations :: Handler [Configuration]
listConfigurations =  IO.liftIO DB.getConfigurations

{- |
    Create a new configuration. See 'API.NewConfiguration'.
-}
newConfiguration :: Configuration -> Handler Configuration
newConfiguration configuration = do
  IO.liftIO (DB.newConfiguration configuration)
  return configuration

{- |
    Try to get a particular configuration. See 'API.ShowConfiguration'.
-}
showConfiguration :: Int -> Handler Configuration
showConfiguration id = withConfiguration id $ \ configuration -> return configuration

{- |
    Update an existing configuration. See 'API.UpdateConfiguration'.
-}
updateConfiguration :: Int -> Configuration -> Handler Configuration
updateConfiguration id newConfiguration = withConfiguration id $ \ configuration -> do
  IO.liftIO (DB.updateConfiguration id newConfiguration)
  return newConfiguration

{- |
    Delete an existing configuration. See 'API.DeleteConfiguration'.
-}
deleteConfiguration :: Int -> Handler Configuration
deleteConfiguration id = withConfiguration id $ \ configuration -> do
  IO.liftIO (DB.deleteConfiguration id)
  return configuration

{- |
    Try to get a particular configuration. See 'API.ShowConfiguration'.
-}
withConfiguration :: Int -> (Configuration -> Handler a) -> Handler a
withConfiguration id go = do
  maybeConfiguration  <- IO.liftIO (DB.getConfiguration id)
  case maybeConfiguration of
    Nothing -> throwError Servant.err404
    Just configuration -> go configuration
