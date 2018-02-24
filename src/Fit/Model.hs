{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Fit.Model
  ( Command(..)
  , Suggestion(..)
  , suggest
  ) where

import           Brick.Widgets.Core (TextWidth)
import           Control.Newtype
import           Data.Text.Zipper.Generic
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

suggest :: Command -> [Suggestion]
suggest = const $ Suggestion <$> ["Foo", "Bar", "Baz"]
