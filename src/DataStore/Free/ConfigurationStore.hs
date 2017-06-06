
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

import Database.Persist ( Key, ToBackendKey, delete, get, insert, replace )
import Database.Persist.Sqlite ( SqlBackend, SqlPersistT, runMigration, runSqlConn, toSqlKey, withSqliteConn )
import Database.Persist.TH ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )
import Control.Monad.IO.Class (liftIO)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, catch, throwM)
import Data.Text (Text)

import Control.Monad.Trans.Either (EitherT)
import Servant ( ServantErr(..), Server, err404, serve )

import Control.Monad.Operational (Program, ProgramViewT(..), singleton, view)

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
      [persistLowerCase|
Configuration json
    name Text
    deriving Show
|]


{- | The whole point of this ""free-monad" example is the next couple of
     lines.  We are defining a typeclass that represents actions that can be
     performed on a database
-}
type ConfigurationStoreDSL = Program StoreAction
data StoreAction a where
  ThrowDb  :: ServantErr -> StoreAction a
  GetConfiguration :: Key Configuration -> StoreAction (Maybe Configuration)
  InsertConfiguration :: Configuration -> StoreAction (Key Configuration)
  UpdateConfiguration :: Key Configuration -> Configuration -> StoreAction()
  DeleteConfiguration :: Key Configuration -> StoreAction()

{-- database dsl interpreter --}
runStoreDSL ::  ConfigurationStoreDSL c -> SqlPersistT (EitherT ServantErr IO) c
runStoreDSL dslStore =
  case view dslStore of
    Return a -> return a
    -- This evaluates a 'GetConfiguration' request to actually get a 'Configuration' from the database.
    GetConfiguration key :>>= next -> do
      -- 'get' is a function from Persistent that gets a 'Configuration' from the database given a key.
      maybeVal <- get key
      runStoreDSL $ next maybeVal
    -- Evaluate a 'InsertDb' request to insert a 'Configuration' in to the database.
    InsertConfiguration configuration :>>= next -> do
      key <- insert configuration
      runStoreDSL $ next key
    -- Evaluate a 'UpdateConfiguration request to update a 'Configuration' in the database.
    UpdateConfiguration key configuration :>>= next -> do
      replace key configuration
      runStoreDSL $ next()
    -- Evaluate a 'DeleteConfiguration' request to delete a 'Configuration' from the  database.
    DeleteConfiguration key :>>= next -> do
      delete key
      runStoreDSL $ next()
    -- Throw an error to indicate that something went wrong.
    ThrowDb servantErr :>>= _ ->
      -- In actual usage, you may need to rollback the database
      -- transaction here.  It doesn't matter for this simple
      -- demonstration, but in production you'll probably want to roll
      -- back the current transaction when you use 'Throw'.
      -- conn <- ask
      -- liftIO $ connRollback conn (getStmtConn conn)
      throwM servantErr
