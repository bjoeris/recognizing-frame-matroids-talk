module Halogen.HTML.Styles.Tweened where

import Prelude

import Halogen.HTML.Properties.Indexed (IProp(), I())
import Halogen.HTML.Styles.Indexed as I
import Halogen.HTML.Indexed as HH

import Halogen.CSS.Colors (class IsColor, toColorString)

import Timeline.Tween (TTween)

type StyleProp b r i = TTween b (IProp (style :: I | r) i)

style :: forall b r i. String -> StyleProp b r i
style = I.style >>> pure >>> pure 

opacity :: forall b r i. Number -> StyleProp b r i
opacity = I.opacity >>> pure >>> pure

backgroundColor :: forall v b r i. (IsColor v) => v -> StyleProp b r i
backgroundColor = I.backgroundColor >>> pure >>> pure