module Fit.Event
  ( FitEvent(..)
  ) where

import           Fit.Model
import           Protolude

newtype FitEvent = SuggestionSelected Suggestion
  deriving (Eq, Show)
