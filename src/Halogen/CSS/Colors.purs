module Halogen.CSS.Colors where

class IsColor c where
  toColorString :: c -> String

instance isColorString :: IsColor String where
  toColorString s = s