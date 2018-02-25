module Fit.Event
  ( FitEvent(..)
  ) where

import           Fit.Model (Command)
import           Protolude

newtype FitEvent = CommandSelected Command
  deriving (Eq, Show)
