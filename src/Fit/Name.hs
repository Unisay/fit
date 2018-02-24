module Fit.Name
  ( FitName(..)
  ) where

import Protolude

data FitName
  = CmdEdit
  | SuggestionsList
  deriving (Eq, Ord, Show)
