module Main where

import Server
import Api

import qualified Control.Exception as Exception
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
{- |
    Launch the web server. Use Ctrl-C to stop it.
-}

main :: IO()
main = do
  let port = 8080 :: Int
  putStrLn ("Starting on port " ++ show port ++ "...")
  Exception.catch
    (Warp.run port application)
    (\ Exception.UserInterrupt -> putStrLn "\nStopping...")

{- |
    The Servant API as a WAI application.
-}
application :: Wai.Application
application = Servant.serve Api.api Server.server
