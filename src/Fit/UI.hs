module Fit.UI
  ( display
  ) where

import           Brick
import qualified Brick.Focus                as F
import qualified Brick.Main                 as M
import           Brick.Themes               (Theme, loadCustomizations,
                                             newTheme, themeToAttrMap)
import           Brick.Types                (BrickEvent (..), handleEventLensed)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as S
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import           Control.Newtype
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import           Fit.Config
import           Fit.Model
import           Fit.Name
import           Fit.State
import qualified Graphics.Vty               as V
import           Graphics.Vty.Attributes
import           Lens.Micro.Platform
import           Protolude

display :: FitConfig -> Maybe Text -> IO ()
display cfg cmd = do
  theme <- loadedTheme
  void $ defaultMain (app theme) $ initialState cfg cmd

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

type FitEvent = ()

initialState :: FitConfig -> Maybe Text -> FitState
initialState config mbCommand =
  FitState { _focusRing = F.focusRing [CmdEdit, SuggestionsList]
           , _command = E.editor CmdEdit Nothing cmd
           , _suggestions = L.list SuggestionsList (Vec.fromList ss) 1
           }
  where
    ss = suggestCommand config cmd
    cmd = Command $ fromMaybe "" mbCommand


app :: Theme -> App FitState FitEvent FitName
app theme =
  App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
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
      (str "Command: " <+> vLimit 1 (commandEditor st))
        <=> B.hBorder
        <=> suggestionsList st

commandEditor :: FitState -> Widget FitName
commandEditor st = F.withFocusRing
  (st ^. focusRing)
  (E.renderEditor (txt . T.unlines . fmap unpack))
  (st ^. command)

suggestionsList :: FitState -> Widget FitName
suggestionsList st = F.withFocusRing
  (st ^. focusRing)
  (L.renderList renderSuggestion)
  (st ^. suggestions)

suggestionAttr :: AttrName
suggestionAttr = attrName "suggestion"

suggestionNameAttr :: AttrName
suggestionNameAttr = suggestionAttr <> "name"

suggestionDescAttr :: AttrName
suggestionDescAttr = suggestionAttr <> "desc"

renderSuggestion :: Bool -> Suggestion -> Widget FitName
renderSuggestion isSelected (Suggestion (Name name) (Description desc)) =
  suggestion  where
    suggestion = withAttr suggA $ nameW <+> descW
    nameW = padLeft (Pad 1) $ padRight Max $ hLimit 12 $ withAttr nameA $ txt name
    descW = padRight (Pad 1) $ padLeft Max $ withAttr descA $ txt desc
    suggA = withSelection suggestionAttr
    nameA = withSelection suggestionNameAttr
    descA = withSelection suggestionDescAttr
    withSelection attr = bool attr (selected attr) isSelected

handleEvent :: FitState -> BrickEvent FitName FitEvent -> EventM FitName (Next FitState)
handleEvent st (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
    _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
           Just CmdEdit -> handleEventLensed st command E.handleEditorEvent ev
           Just SuggestionsList -> handleEventLensed st suggestions L.handleListEvent ev
           Nothing -> return st

handleEvent st _ = continue st
