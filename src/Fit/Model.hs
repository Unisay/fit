{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Fit.Model
  ( Command(..)
  , Suggestion(..)
  , Name(..)
  , Description(..)
  , suggestCommand
  ) where

import           Brick.Widgets.Core       (TextWidth)
import           Control.Newtype
import           Data.Text.Zipper.Generic
import           Fit.Config
import           Protolude

newtype Command = Command Text
  deriving (Eq, Show, Monoid, GenericTextZipper, TextWidth)

instance Newtype Command Text where
  pack = Command
  unpack (Command t) = t

newtype Name = Name Text
  deriving (Eq, Show)

newtype Description = Description Text
  deriving (Eq, Show)

data Suggestion = Suggestion Name Description
  deriving (Eq, Show)

suggestCommand :: FitConfig -> Command -> [Suggestion]
suggestCommand (FitConfig commands) _ =
  suggest <$> commands
  where
    suggest (CommandConfig name desc) =
      Suggestion (Name name) (Description desc)
