module Halogen.HTML.Styles.Tweened where

import Prelude

import Halogen.HTML.Properties.Indexed (IProp(), I())
import Halogen.HTML.Styles.Indexed as I
import Halogen.HTML.Indexed as HH

import Halogen.CSS.Colors (class IsColor, toColorString)
import Halogen.CSS.Units (class IsLength, toLengthString)

import Timeline.Tween (TTween)

type StyleProp b r i = TTween b (IProp (style :: I | r) i)

style :: forall b r i. String -> StyleProp b r i
style = I.style >>> pure >>> pure 

opacity :: forall b r i. Number -> StyleProp b r i
opacity = I.opacity >>> pure >>> pure

backgroundColor :: forall v b r i. (IsColor v) => v -> StyleProp b r i
backgroundColor = I.backgroundColor >>> pure >>> pure

type Position = I.Position

static :: Position
static = I.static

relative :: Position
relative = I.relative

absolute :: Position
absolute = I.absolute

fixed :: Position
fixed = I.fixed

sticky :: Position
sticky = I.sticky

position :: forall b r i. Position -> StyleProp b r i
position = I.position >>> pure >>> pure

width :: forall v b r i. IsLength v => v -> StyleProp b r i
width = I.width >>> pure >>> pure

height :: forall v b r i. IsLength v => v -> StyleProp b r i
height = I.height >>> pure >>> pure

left :: forall v b r i. IsLength v => v -> StyleProp b r i
left = I.left >>> pure >>> pure

right :: forall v b r i. IsLength v => v -> StyleProp b r i
right = I.right >>> pure >>> pure

top :: forall v b r i. IsLength v => v -> StyleProp b r i
top = I.top >>> pure >>> pure

bottom :: forall v b r i. IsLength v => v -> StyleProp b r i
bottom = I.bottom >>> pure >>> pure

marginLeft :: forall v b r i. IsLength v => v -> StyleProp b r i
marginLeft = I.marginLeft >>> pure >>> pure

marginRight :: forall v b r i. IsLength v => v -> StyleProp b r i
marginRight = I.marginRight >>> pure >>> pure

marginTop :: forall v b r i. IsLength v => v -> StyleProp b r i
marginTop = I.marginTop >>> pure >>> pure

marginBottom :: forall v b r i. IsLength v => v -> StyleProp b r i
marginBottom = I.marginBottom >>> pure >>> pure