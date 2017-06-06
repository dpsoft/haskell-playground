{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
   The core data type for this example application.
-}
module Model where

import Data.Aeson as Aeson

import qualified GHC.Generics as GHC
import qualified Data.Text as Text

{- |
    A Configuration. Who knows what it does.
-}
data Configuration = Configuration { name :: Text.Text } -- ^ The name of this Configuration.
                    deriving (Aeson.FromJSON, Aeson.ToJSON, Eq, GHC.Generic, Read, Show)
