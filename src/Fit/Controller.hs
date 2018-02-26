module Fit.Controller
  ( handleAppEvent
  ) where

import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.List  as L
import qualified Data.Text.Zipper    as Z
import qualified Data.Vector         as V
import           Fit.Event
import           Fit.Model
import           Fit.State
import           Lens.Micro.Platform
import           Protolude

handleAppEvent :: FitState -> FitEvent -> FitState
handleAppEvent st (SuggestionSelected (CommandSuggestion cmd)) =
  let cfg = st^.configL
      st' = st & (commandLineL . commandL) .~ Just cmd
      cl = st'^.commandLineL
      line = renderCommandLine cl
      suggestions = V.fromList $ suggest cfg cl
  in st' & (commandEditorL . E.editContentsL) %~ (Z.insertMany line . Z.clearZipper)
         & suggestionsL %~ L.listReplace suggestions Nothing
handleAppEvent st _ = st

