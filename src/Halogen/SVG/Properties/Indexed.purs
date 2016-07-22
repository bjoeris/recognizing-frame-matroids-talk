module Halogen.SVG.Properties.Indexed where

import Halogen.HTML.Properties.Indexed (IProp(), I())
import Halogen.HTML.Properties.Indexed.Internal (refine)

import Halogen.SVG.Properties as P

import Halogen.CSS.Units (class IsLength)
import Halogen.CSS.Colors (class IsColor)

import Unsafe.Coerce (unsafeCoerce)

d :: forall r i. String -> IProp ( d :: I | r) i
d = refine P.d

cx :: forall v r i. IsLength v => v -> IProp ( cx :: I | r) i
cx = refine P.cx

cy :: forall v r i. IsLength v => v -> IProp ( cy :: I | r) i
cy = refine P.cy

r :: forall v r i. IsLength v => v -> IProp ( r :: I | r) i
r = refine P.r

width :: forall v r i. IsLength v => v -> IProp ( width :: I | r) i
width = refine P.width

height :: forall v r i. IsLength v => v -> IProp ( height :: I | r) i
height = refine P.height

fill :: forall v r i. IsColor v => v -> IProp ( fill :: I | r) i
fill = refine P.fill

stroke :: forall v r i. IsColor v => v -> IProp ( stroke :: I | r) i
stroke = refine P.stroke

strokeWidth :: forall v r i. IsLength v => v -> IProp ( strokeWidth :: I | r) i
strokeWidth = refine P.strokeWidth

filter :: forall r i. String -> IProp ( filter :: I | r) i
filter = refine P.filter

id :: forall r i. String -> IProp ( id :: I | r) i
id = refine P.id

x :: forall v r i. IsLength v => v -> IProp ( x :: I | r) i
x = refine P.x

x1 :: forall v r i. IsLength v => v -> IProp ( x1 :: I | r) i
x1 = refine P.x1

x2 :: forall v r i. IsLength v => v -> IProp ( x2 :: I | r) i
x2 = refine P.x2

y :: forall v r i. IsLength v => v -> IProp ( y :: I | r) i
y = refine P.y

y1 :: forall v r i. IsLength v => v -> IProp ( y1 :: I | r) i
y1 = refine P.y1

y2 :: forall v r i. IsLength v => v -> IProp ( y2 :: I | r) i
y2 = refine P.y2

in_ :: forall r i. String -> IProp ( in :: I | r) i
in_ = refine P.in_

stdDeviation :: forall r i. Number -> IProp ( stdDeviation :: I | r) i
stdDeviation = refine P.stdDeviation

clipPath :: forall r i. String -> IProp ( clipPath :: I | r) i
clipPath = refine P.clipPath

strokeLinecap :: forall r i. P.Linecap -> IProp (strokeLinecap :: I | r) i
strokeLinecap = refine P.strokeLinecap

strokeLinejoin :: forall r i. P.Linejoin -> IProp (strokeLinejoin :: I | r) i
strokeLinejoin = refine P.strokeLinejoin

offset :: forall r i. Number -> IProp ( offset :: I | r) i
offset = refine P.offset

stopColor :: forall v r i. IsColor v => v -> IProp ( stopColor :: I | r) i
stopColor = refine P.stopColor

viewBox :: forall r i. Number -> Number -> Number -> Number -> IProp ( viewBox :: I | r) i
viewBox = unsafeCoerce P.viewBox

preserveAspectRatio :: forall r i. P.PreserveAspectRatio -> P.PreserveAspectRatio -> IProp (preserveAspectRatio :: I | r) i
preserveAspectRatio = unsafeCoerce P.preserveAspectRatio

gradientUnits :: forall r i. P.GradientUnits -> IProp (gradientUnits :: I | r) i
gradientUnits = unsafeCoerce P.gradientUnits

type ConditionalProcessingAttributes r =
  ( requiredFeatures :: I
  , requiredExtensions :: I
  , systemLanguage :: I
  | r
  )
  
type CoreAttributes r =
  ( id :: I
  , xml_base :: I
  , xml_lang :: I
  , xml_space :: I
  | r
  )
  
type GraphicalEventAttributes r =
  ( onfocusin :: I
  , onfocusout :: I
  , onactivate :: I
  , onclick :: I
  , onmousedown :: I
  , onmouseup :: I
  , onmouseover :: I
  , onmousemove :: I
  , onmouseout :: I
  , onload :: I
  | r
  )
  
type PresentationAttributes r =
  ( alignmentBaseline :: I
  , baselineShift :: I
  , clip :: I
  , clipPath :: I
  , clipRule ::I
  , color :: I
  , colorInterpolation :: I
  , colorInterpolationFilters :: I
  , colorProfile :: I
  , colorRendering :: I
  , cursor :: I
  , direction :: I
  , display :: I
  , dominantBaseline :: I
  , enableBackground :: I
  , fill :: I
  , fillOpacity :: I
  , fillRule :: I
  , filter :: I
  , floodColor :: I
  , floodOpacity :: I
  , fontFamily :: I
  , fontSize :: I
  , fontSizeAdjust :: I
  , fontStretch :: I
  , fontStyle :: I
  , fontVariant :: I
  , fontWeight :: I
  , glyphOrientationHorizontal :: I
  , glyphOrientationVertical :: I
  , imageRendering :: I
  , kerning :: I
  , letterSpacing :: I
  , lightingColor :: I
  , markerEnd :: I
  , markerMid :: I
  , markerStart :: I
  , mask :: I
  , opacity :: I
  , overflow :: I
  , pointerEvents :: I
  , shapeRendering :: I
  , stopColor :: I
  , stopOpacity :: I
  , stroke :: I
  , strokeDasharray :: I
  , strokeDashoffset :: I
  , strokeLinecap :: I
  , strokeLinejoin :: I
  , strokeMiterlimit :: I
  , strokeOpacity :: I
  , strokeWidth :: I
  , textAnchor :: I
  , textDecoration :: I
  , textRendering :: I
  , unicodeBidi :: I
  , visibility :: I
  , wordSpacing :: I
  , writingMode :: I
  | r
  )

type DocumentEventAttributes r = 
  ( onabort :: I
  , onerror :: I
  , onresize :: I
  , onscroll :: I
  , onunload :: I
  , onzoom :: I
  | r)

type XLinkAttributes r = r 
-- TODO: xlink:href, xlink:type, xlink:role, xlink:arcrole, xlink:title, xlink:show, xlink:actuate

type GlobalAttributes r = 
  ( ConditionalProcessingAttributes 
  ( CoreAttributes 
  ( GraphicalEventAttributes 
  ( PresentationAttributes 
  ( class :: I
  , style :: I
  , externalResourceRequired :: I
  | r )))))