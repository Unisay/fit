{-# LANGUAGE TemplateHaskell #-}

module Fit.State
  ( FitState(..)
  , focusRing
  , command
  , suggestions
  ) where

import qualified Brick.Focus         as F
import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.List  as L
import           Fit.Model
import           Fit.Name
import           Lens.Micro.Platform


data FitState = FitState
  { _focusRing   :: F.FocusRing FitName
  , _command     :: E.Editor Command FitName
  , _suggestions :: L.List FitName Suggestion
  }

makeLenses ''FitState
