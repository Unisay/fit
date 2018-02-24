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
import qualified Brick.Widgets.Edit         as E
import qualified Data.Text                  as T
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
initialState cmd =
  FitState { _focusRing = F.focusRing [CmdEdit]
           , _command = cmd
           , _cmdEdit = E.editor CmdEdit Nothing ""
           }


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
    [ (E.editAttr,                   black `on` white)
    , (E.editFocusedAttr,            black `on` blue)
    ]

ui :: FitState -> Widget FitName
ui st = withBorderStyle S.unicode $
  B.borderWithLabel (str "Forget it!") $
    str "Command: " <+> vLimit 1 e1
  where
   e1 = F.withFocusRing (st ^. focusRing)
                        (E.renderEditor (str . T.unpack . T.unlines))
                        (st ^. cmdEdit)

handleEvent :: FitState -> BrickEvent FitName FitEvent -> EventM FitName (Next FitState)
handleEvent st (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev

    _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
           Just CmdEdit -> handleEventLensed st cmdEdit E.handleEditorEvent ev
           Nothing -> return st

handleEvent st _            = continue st
