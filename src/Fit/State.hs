{-# LANGUAGE TemplateHaskell #-}

module Fit.State
  ( FitState(..)
  , fitEvents
  , focusRing
  , commandLine
  , commandEditor
  , suggestions
  ) where

import qualified Brick.BChan         as C
import qualified Brick.Focus         as F
import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.List  as L
import           Fit.Event
import           Fit.Model
import           Fit.Name
import           Lens.Micro.Platform
import           Protolude

data FitState = FitState
  { _fitEvents     :: C.BChan FitEvent
  , _focusRing     :: F.FocusRing FitName
  , _commandLine   :: CommandLine
  , _commandEditor :: E.Editor Text FitName
  , _suggestions   :: L.List FitName (Suggestion Command)
  }

makeLenses ''FitState
