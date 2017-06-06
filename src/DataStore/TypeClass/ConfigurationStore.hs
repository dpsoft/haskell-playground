{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module ConfigurationStore where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Exception (Exception)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Catch (MonadThrow, catch, throwM)
import Data.Text (Text)
import Control.Monad.Trans.Either (EitherT)
import Servant ( ServantErr(..), Server, err404, serve )

--import Model
share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
      [persistLowerCase|
Configuration json
    name Text
    deriving Show
|]

{- Typeclass for accessing a database -}

{- | The whole point of this "typeclass" example is the next couple of
     lines.  We are defining a typeclass that represents actions that can be
     performed on a database
-}
class Monad m => DataStore m where
  getConfiguration :: Key Configuration -> m (Maybe Configuration)
  insertConfiguration :: Configuration -> m (Key Configuration)
  updateConfiguration :: Key Configuration -> Configuration -> m()
  deleteConfiguration :: Key Configuration -> m()
  runStore :: m a -> EitherT ServantErr IO a

{-| DataStore typeclass instance -}

{- | Here is our instance of 'DataStore' for accessing the database in
     production. It pretty much just directly wraps the calls to Persistent.
-}
instance DataStore (SqlPersistT IO) where
  getConfiguration :: Key Configuration -> SqlPersistT IO (Maybe Configuration)
  getConfiguration  = get

  insertConfiguration :: Configuration -> SqlPersistT IO (Key Configuration)
  insertConfiguration = insert

  updateConfiguration :: Key Configuration -> Configuration -> SqlPersistT IO ()
  updateConfiguration = replace

  deleteConfiguration :: Key Configuration -> SqlPersistT IO()
  deleteConfiguration = delete

  runStore :: SqlBackend -> SqlPersistT IO a -> EitherT ServantErr IO a
  runStore conn query =
    liftIO (runSqlConn query conn) `catch` \(err::ServantErr) -> throwError err
