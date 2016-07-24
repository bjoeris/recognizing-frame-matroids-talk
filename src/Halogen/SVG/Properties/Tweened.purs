module Halogen.SVG.Properties.Tweened where

import Prelude

import Halogen.HTML.Properties.Indexed (IProp(), I())
import Halogen.HTML.Properties.Tweened (tweenProp)
import Halogen.HTML.Indexed as HH

import Halogen.SVG.Properties as P
import Halogen.SVG.Properties.Indexed as I

import Halogen.CSS.Units (class IsLength)
import Halogen.CSS.Colors (class IsColor)

import Timeline.Tween (TTween)

class_ :: forall b r i. String -> TTween b (IProp ( class :: I | r) i)
class_ = I.class_ >>> pure >>> pure

d :: forall b r i. String -> TTween b (IProp ( d :: I | r) i)
d = I.d >>> pure >>> pure

cx :: forall b v r i. IsLength v => v -> TTween b (IProp ( cx :: I | r) i)
cx = I.cx >>> pure >>> pure

cy :: forall b v r i. IsLength v => v -> TTween b (IProp ( cy :: I | r) i)
cy = I.cy >>> pure >>> pure

r :: forall b v r i. IsLength v => v -> TTween b (IProp ( r :: I | r) i)
r = I.r >>> pure >>> pure

width :: forall b v r i. IsLength v => v -> TTween b (IProp ( width :: I | r) i)
width = I.width >>> pure >>> pure

height :: forall b v r i. IsLength v => v -> TTween b (IProp ( height :: I | r) i)
height = I.height >>> pure >>> pure

fill :: forall b v r i. IsColor v => v -> TTween b (IProp ( fill :: I | r) i)
fill = I.fill >>> pure >>> pure

fillOpacity :: forall b r i. Number -> TTween b (IProp ( fillOpacity :: I | r) i)
fillOpacity = I.fillOpacity >>> pure >>> pure

stroke :: forall b v r i. IsColor v => v -> TTween b (IProp ( stroke :: I | r) i)
stroke = I.stroke >>> pure >>> pure

strokeWidth :: forall b v r i. IsLength v => v -> TTween b (IProp ( strokeWidth :: I | r) i)
strokeWidth = I.strokeWidth >>> pure >>> pure

filter :: forall b r i. String -> TTween b (IProp ( filter :: I | r) i)
filter = I.filter >>> pure >>> pure

id :: forall b r i. String -> TTween b (IProp ( id :: I | r) i)
id = I.id >>> pure >>> pure

x :: forall b v r i. IsLength v => v -> TTween b (IProp ( x :: I | r) i)
x = I.x >>> pure >>> pure

y :: forall b v r i. IsLength v => v -> TTween b (IProp ( y :: I | r) i)
y = I.y >>> pure >>> pure

in_ :: forall b r i. String -> TTween b (IProp ( in :: I | r) i)
in_ = I.in_ >>> pure >>> pure

stdDeviation :: forall b r i. Number -> TTween b (IProp ( stdDeviation :: I | r) i)
stdDeviation = I.stdDeviation >>> pure >>> pure

clipPath :: forall b r i. String -> TTween b (IProp ( clipPath :: I | r) i)
clipPath = I.clipPath >>> pure >>> pure

strokeLinecap :: forall b r i. P.Linecap -> TTween b (IProp (strokeLinecap :: I | r) i)
strokeLinecap = I.strokeLinecap >>> pure >>> pure

strokeLinejoin :: forall b r i. P.Linejoin -> TTween b (IProp (strokeLinejoin :: I | r) i)
strokeLinejoin = I.strokeLinejoin >>> pure >>> pure

preserveAspectRatio :: forall b r i. P.PreserveAspectRatio -> TTween b (IProp (preserveAspectRatio :: I | r) i)
preserveAspectRatio = I.preserveAspectRatio >>> pure >>> pure

viewBox :: forall b r i. Number -> Number -> Number -> Number -> TTween b (IProp (viewBox :: I | r) i)
viewBox x y width height = I.viewBox x y width height # pure # pure