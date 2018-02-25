{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Fit.Model
  ( CommandLine(..)
  , Command(..)
  , Suggestion(..)
  , Description(..)
  , Named
  , name
  , suggestCommand
  , command
  , subCommand
  , flags
  , emptyCommandLine
  ) where

import Brick.Widgets.Core (TextWidth)
import Control.Newtype
import Data.Text.Zipper.Generic
import Fit.Config
import Lens.Micro.Platform
import Protolude

type Name = Text

class Named a  where
  name :: a -> Text

newtype Command =
  Command Name
  deriving (Eq, Show, Monoid, GenericTextZipper, TextWidth)

instance Newtype Command Text where
  pack = Command
  unpack (Command t) = t

instance Named Command where
  name (Command n) = n

data Flag =
  Flag Name
       Description
  deriving (Eq, Show)

instance Named Flag where
  name (Flag n _) = n

data SubCommand =
  SubCommand Name
             Description
  deriving (Eq, Show)

instance Named SubCommand where
  name (SubCommand n _) = n

newtype Description =
  Description Text
  deriving (Eq, Show)

data Suggestion e = Suggestion
  { suggested :: e
  , description :: Description
  } deriving (Eq, Show)

instance Named e =>
         Named (Suggestion e) where
  name (Suggestion e _) = name e

suggestCommand :: FitConfig -> Command -> [Suggestion Command]
suggestCommand (FitConfig commands) _ = suggest <$> commands
  where
    suggest (CommandConfig nm ds) = Suggestion (Command nm) (Description ds)

data CommandLine = CommandLine
  { _command :: Maybe Command
  , _subCommand :: Maybe SubCommand
  , _flags :: [Flag]
  } deriving (Show, Eq)

emptyCommandLine :: CommandLine
emptyCommandLine =
  CommandLine
  { _command = Nothing
  , _subCommand = Nothing
  , _flags = []
  }

makeLenses ''CommandLine
