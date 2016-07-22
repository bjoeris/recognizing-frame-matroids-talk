module Halogen.HTML.Styles.Indexed where

import Halogen.HTML.Styles as P
import Halogen.HTML.Properties.Indexed (IProp(), I())

import Halogen.HTML.Properties.Indexed.Internal (refine)

import Halogen.CSS.Colors (class IsColor, toColorString)

style :: forall r i. String -> IProp ( style :: I | r) i
style = refine P.style

opacity :: forall r i. Number -> IProp ( style :: I | r) i
opacity = refine P.opacity

backgroundColor :: forall v r i. IsColor v => v -> IProp ( style :: I | r) i
backgroundColor = refine P.backgroundColor