{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Fit.Config
  ( FitConfig(..)
  , CommandConfig(..)
  ) where

import qualified Dhall     as D
import           Protolude

data CommandConfig = CommandConfig
  { commandName        :: Text
  , commandDescription :: Text
  } deriving (Generic, D.Interpret, Show)

newtype FitConfig = FitConfig
  { configuredCommands :: [CommandConfig]
  } deriving (Generic, D.Interpret, Show)
