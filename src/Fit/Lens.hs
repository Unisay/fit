module Fit.Lens (customLensRules) where

import           Data.Char           (toLower)
import           Language.Haskell.TH
import           Lens.Micro.Platform
import           Protolude

customLensRules :: LensRules
customLensRules =
  lensRules
    & simpleLenses .~ True
    & lensField    .~ \_ _ n ->
      case nameBase n of
        '_':x:xs -> [TopName (mkName $ (toLower x : xs) ++ "L")]
        _        -> []
