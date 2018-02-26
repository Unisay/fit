{-# LANGUAGE TemplateHaskell #-}

module Fit.State
  ( FitState
  , initialState
  , configL
  , fitEventsL
  , focusRingL
  , commandLineL
  , commandEditorL
  , suggestionsL
  ) where

import qualified Brick.BChan         as C
import qualified Brick.Focus         as F
import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.List  as L
import qualified Data.Vector         as Vec
import           Fit.Config
import           Fit.Event
import           Fit.Lens            (customLensRules)
import           Fit.Model
import           Fit.Name
import           Lens.Micro.Platform
import           Protolude

data FitState = FitState
  { _config        :: FitConfig
  , _fitEvents     :: C.BChan FitEvent
  , _focusRing     :: F.FocusRing FitName
  , _commandLine   :: CommandLine
  , _commandEditor :: E.Editor Text FitName
  , _suggestions   :: L.List FitName Suggestion
  }

makeLensesWith customLensRules ''FitState

initialState :: C.BChan FitEvent -> FitConfig -> Maybe Text -> FitState
initialState chan config mbCommand =
  FitState { _config = config
           , _fitEvents = chan
           , _focusRing = F.focusRing [SuggestionsList, CmdEdit]
           , _commandLine = emptyCommandLine
           , _commandEditor = E.editor CmdEdit Nothing cmd
           , _suggestions = L.list SuggestionsList (Vec.fromList ss) 1
           }
  where
    ss = suggest config emptyCommandLine -- todo: user input
    cmd = fromMaybe "" mbCommand

