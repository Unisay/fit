{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Fit.Model
  ( Command(..)
  , Suggestion(..)
  , suggestCommand
  ) where

import           Brick.Widgets.Core       (TextWidth)
import           Control.Newtype
import           Data.Text.Zipper.Generic
import           Fit.Config
import           Protolude

newtype Command =
  Command Text
  deriving (Eq, Show, Monoid, GenericTextZipper, TextWidth)

instance Newtype Command Text where
  pack = Command
  unpack (Command t) = t


newtype Suggestion =
  Suggestion Text
  deriving (Eq, Show)

suggestCommand :: FitConfig -> Command -> [Suggestion]
suggestCommand (FitConfig commands) = const $ suggest <$> commands where
  suggest (CommandConfig name desc) = Suggestion $ name <> " - " <> desc
