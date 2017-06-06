{- |
    A dummy Database for testing purposes
-}
module Database where

import qualified System.IO.Unsafe       as Unsafe
import qualified Control.Concurrent.STM as STM
import           Model
import           Safe


{-|
  Our Database
-}
newtype Database = Database { configurations :: [Configuration] } deriving (Eq, Read, Show)

{-|
  The default (empty) database.
-}
defaultDatabase :: Database
defaultDatabase = Database {configurations = []}

{-|
  A reference to our Database, Implemented with 'Unsafe.unsafePerformIO'
-}
database :: STM.TVar Database
database = Unsafe.unsafePerformIO (STM.newTVarIO defaultDatabase)


{- |
    Get all of the configurations.
-}
getConfigurations :: IO[Configuration]
getConfigurations = do
  db <- STM.readTVarIO database
  return (configurations db)

{- |
    Try to get a particular configurations.
-}
getConfiguration :: Int -> IO(Maybe Configuration)
getConfiguration id = do
  let index = id - 1
  fmap (flip Safe.atMay index) getConfigurations

{- |
    Create a new configuration.
-}
newConfiguration :: Configuration -> IO()
newConfiguration configuration =
  STM.atomically $
    STM.modifyTVar database $ \db -> db { configurations = configurations db ++ [configuration]}

{- |
    Update an existing configuration.
-}
updateConfiguration :: Int -> Configuration -> IO()
updateConfiguration id configuration =
  STM.atomically $
    STM.modifyTVar database $ \db ->
                               db { configurations =
                                  map(\ (i, x) -> if id == i then configuration else x)
                                    (zip [1..](configurations db))
                                }


{- |
  Delete an existing configuration.
-}
deleteConfiguration :: Int -> IO()
deleteConfiguration id =
  STM.atomically $
    STM.modifyTVar database $ \db ->
                               db { configurations =
                                  map snd (filter (\ (i, _) -> i /= id)
                                    (zip [1..](configurations db)))
                                  }
