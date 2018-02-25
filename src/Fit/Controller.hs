module Fit.Controller
  ( handleAppEvent
  ) where

import qualified Brick.Widgets.Edit  as E
import qualified Data.Text.Zipper    as Z
import           Fit.Event
import           Fit.Model
import           Fit.State
import           Lens.Micro.Platform
import           Protolude

handleAppEvent :: FitState -> FitEvent -> FitState
handleAppEvent st (CommandSelected c@(Command t)) =
  st & (commandLine . command) .~ Just c
     & (commandEditor . E.editContentsL) %~ (Z.insertMany t . Z.clearZipper)
