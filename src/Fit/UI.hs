module Fit.UI
  ( display
  ) where

import           Brick
import qualified Brick.AttrMap              as A
import qualified Brick.Focus                as F
import qualified Brick.Main                 as M
import           Brick.Types                (BrickEvent (..), handleEventLensed)
import           Brick.Util                 (on)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as S
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import qualified Control.Newtype            as NT
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import           Fit.Model
import           Fit.Name
import           Fit.State
import qualified Graphics.Vty               as V
import           Graphics.Vty.Attributes
import           Lens.Micro.Platform
import           Protolude                  hiding (on)

display :: Maybe Text -> IO ()
display c = void $ defaultMain app (initialState c)

type FitEvent = ()

initialState :: Maybe Text -> FitState
initialState mbCommand =
  FitState { _focusRing = F.focusRing [CmdEdit, SuggestionsList]
           , _command = E.editor CmdEdit Nothing cmd
           , _suggestions = L.list SuggestionsList (Vec.fromList ss) 1
           }
  where
    ss = suggest cmd
    cmd = Command $ fromMaybe "" mbCommand


app :: App FitState FitEvent FitName
app =
  App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const attributes
  }

drawUi :: FitState -> [Widget FitName]
drawUi s = [ui s]

attributes :: AttrMap
attributes = A.attrMap V.defAttr
    [ ( E.editAttr,                   black `on` white )
    , ( E.editFocusedAttr,            black `on` blue  )
    , ( L.listAttr,                   fg white         )
    , ( L.listSelectedAttr,           fg yellow        )
    ]

ui :: FitState -> Widget FitName
ui st =
  withBorderStyle S.unicode $
  B.borderWithLabel (str "Forget it!") $
    (str "Command: " <+> vLimit 1 (commandEditor st)) <=>
    suggestionsList st

commandEditor :: FitState -> Widget FitName
commandEditor st = F.withFocusRing
  (st ^. focusRing)
  (E.renderEditor (txt . T.unlines . fmap NT.unpack))
  (st ^. command)

suggestionsList :: FitState -> Widget FitName
suggestionsList st = F.withFocusRing
  (st ^. focusRing)
  (L.renderList renderSuggestion)
  (st ^. suggestions)

renderSuggestion :: Bool -> Suggestion -> Widget FitName
renderSuggestion sel a =
  let selStr s =
        if sel
          then txt $ "-> " <> s
          else txt s
  in C.hCenter $ selStr (show a)

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
