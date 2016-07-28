module Halogen.HTML.Styles.Indexed where

import Halogen.HTML.Styles as P
import Halogen.HTML.Properties.Indexed (IProp(), I())

import Halogen.HTML.Properties.Indexed.Internal (refine)

import Halogen.CSS.Colors (class IsColor, toColorString)
import Halogen.CSS.Units (class IsLength, toLengthString)

style :: forall r i. String -> IProp ( style :: I | r) i
style = refine P.style

opacity :: forall r i. Number -> IProp ( style :: I | r) i
opacity = refine P.opacity

backgroundColor :: forall v r i. IsColor v => v -> IProp ( style :: I | r) i
backgroundColor = refine P.backgroundColor

type Position = P.Position

static :: Position
static = P.static

relative :: Position
relative = P.relative

absolute :: Position
absolute = P.absolute

fixed :: Position
fixed = P.fixed

sticky :: Position
sticky = P.sticky

position :: forall r i. Position -> IProp ( style :: I | r) i
position = refine P.position

width :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
width = refine P.width

height :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
height = refine P.height

left :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
left = refine P.left

right :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
right = refine P.right

top :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
top = refine P.top

bottom :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
bottom = refine P.bottom

marginLeft :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
marginLeft = refine P.marginLeft

marginRight :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
marginRight = refine P.marginRight

marginTop :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
marginTop = refine P.marginTop

marginBottom :: forall v r i. IsLength v => v -> IProp ( style :: I | r) i
marginBottom = refine P.marginBottom