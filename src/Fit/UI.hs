module Fit.UI
  ( display
  ) where

import           Brick                      hiding (Named)
import qualified Brick.BChan                as C
import qualified Brick.Focus                as F
import           Brick.Themes               (Theme, loadCustomizations,
                                             newTheme, themeToAttrMap)
import           Brick.Types                (BrickEvent (..), handleEventLensed)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as S
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import qualified Data.Text                  as T
import           Fit.Config
import           Fit.Controller
import           Fit.Event
import           Fit.Model
import           Fit.Name
import           Fit.State
import qualified Graphics.Vty               as V
import           Graphics.Vty.Attributes
import           Graphics.Vty.Config        (defaultConfig)
import           Lens.Micro.Platform
import           Protolude

display :: FitConfig -> Maybe Text -> IO ()
display cfg cmd = do
  theme <- loadedTheme
  eventChan <- C.newBChan 10
  let vty = V.mkVty defaultConfig
      st = initialState eventChan cfg cmd
  void $ customMain vty (Just eventChan) (app theme) st

loadedTheme :: IO Theme
loadedTheme = do
  let attrs = [ (E.editAttr,                  defAttr)
              , (E.editFocusedAttr,           defAttr)
              , (L.listAttr,                  defAttr)
              , (L.listSelectedAttr,          defAttr)
              , (suggestionAttr,              defAttr)
              , (suggestionNameAttr,          defAttr)
              , (suggestionDescAttr,          defAttr)
              , (selected suggestionAttr,     styled standout)
              , (selected suggestionNameAttr, styled standout)
              , (selected suggestionDescAttr, styled standout)
              ]
  let defaultTheme = newTheme defAttr attrs
  customizations <- loadCustomizations "theme.ini" defaultTheme
  putErrLn (show customizations :: Text)
  let fallback err = putStr err $> defaultTheme
  either fallback return customizations

selected :: AttrName -> AttrName
selected = flip mappend "selected"

styled :: Style -> Attr
styled = withStyle defAttr

app :: Theme -> App FitState FitEvent FitName
app theme =
  App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleBrickEvent
  , appStartEvent = return
  , appAttrMap = const $ themeToAttrMap theme
  }

drawUi :: FitState -> [Widget FitName]
drawUi s = [ui s]

ui :: FitState -> Widget FitName
ui st =
  withBorderStyle S.unicode $
  B.borderWithLabel (txt "| Forget it! |") $
    padLeftRight 1 $
      (str "Command: " <+> vLimit 1 (commandEditorW st))
        <=> B.hBorder
        <=> suggestionsList st

commandEditorW :: FitState -> Widget FitName
commandEditorW st = F.withFocusRing
  (st ^. focusRingL)
  (E.renderEditor (txt . T.unlines))
  (st ^. commandEditorL)

suggestionsList :: FitState -> Widget FitName
suggestionsList st = F.withFocusRing
  (st ^. focusRingL)
  (L.renderList renderSuggestion)
  (st ^. suggestionsL)

suggestionAttr :: AttrName
suggestionAttr = attrName "suggestion"

suggestionNameAttr :: AttrName
suggestionNameAttr = suggestionAttr <> "name"

suggestionDescAttr :: AttrName
suggestionDescAttr = suggestionAttr <> "desc"

renderSuggestion :: Bool -> Suggestion -> Widget FitName
renderSuggestion isSelected sg = suggestion
  where
    suggestion = withAttr suggA $ nameW <+> descW
    nameW = padLeft (Pad 1) $ padRight Max $ hLimit 12 $ withAttr nameA $ txt $ name sg
    descW = padRight (Pad 1) $ padLeft Max $ withAttr descA $ txt $ description sg
    suggA = withSelection suggestionAttr
    nameA = withSelection suggestionNameAttr
    descA = withSelection suggestionDescAttr
    withSelection attr = bool attr (selected attr) isSelected

handleBrickEvent :: FitState -> BrickEvent FitName FitEvent -> EventM FitName (Next FitState)
handleBrickEvent st (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> halt st
    V.EvKey (V.KChar '\t') [] -> continue $ st & focusRingL %~ F.focusNext
    V.EvKey V.KBackTab [] -> continue $ st & focusRingL %~ F.focusPrev
    _ -> continue =<< case F.focusGetCurrent (st^.focusRingL) of
           Just CmdEdit -> handleEventLensed st commandEditorL E.handleEditorEvent ev
           Just SuggestionsList -> handleSuggestionEvents st ev
           Nothing -> return st
handleBrickEvent st (AppEvent ev) = continue $ handleAppEvent st ev
handleBrickEvent st _ = continue st


handleSuggestionEvents :: FitState -> V.Event -> EventM FitName FitState
handleSuggestionEvents st (V.EvKey V.KEnter []) = do
  let selectedSuggestion = snd <$> L.listSelectedElement (st^.suggestionsL)
      mbEvent = SuggestionSelected <$> selectedSuggestion
      sendEvent = C.writeBChan (st^.fitEventsL)
      doNothing = return ()
  liftIO $ maybe doNothing sendEvent mbEvent
  return st
handleSuggestionEvents st ev =
  handleEventLensed st suggestionsL L.handleListEvent ev
