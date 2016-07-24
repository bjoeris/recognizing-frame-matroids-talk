module Halogen.CSS.Colors where

import Data.Exists

class IsColor c where
  toColorString :: c -> String

instance isColorString :: IsColor String where
  toColorString s = s

data ColorF c = ColorF c (c -> String)

type Color = Exists ColorF

runColorString :: Color -> String
runColorString = runExists (\(ColorF c s) -> s c)

mkColor :: forall c. IsColor c => c -> Color
mkColor c = mkExists (ColorF c toColorString)

instance isColorColor :: IsColor (Exists ColorF) where
  toColorString = runColorString