{-# LANGUAGE TemplateHaskell #-}

module Fit.State
  ( FitState(..)
  , focusRing
  , command
  , cmdEdit
  ) where

import           Lens.Micro.Platform
import           Fit.Name
import           Protolude

import qualified Brick.Focus        as F
import qualified Brick.Widgets.Edit as E


data FitState = FitState
  { _focusRing :: F.FocusRing FitName
  , _command   :: Maybe Text
  , _cmdEdit   :: E.Editor Text FitName
  }

makeLenses ''FitState
