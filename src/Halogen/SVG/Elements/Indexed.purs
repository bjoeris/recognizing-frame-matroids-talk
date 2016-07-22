module Halogen.SVG.Elements.Indexed where

import Halogen.HTML.Elements.Indexed.Internal (refine)
import Halogen.HTML.Properties.Indexed (IProp(), I())

import Halogen.SVG.Properties.Indexed
import Halogen.SVG.Elements as E
import Halogen.SVG.Elements (SVG)

-- | An SVG element that admits children.
type Node r p i
   = Array (IProp r i)
  -> Array (SVG p i)
  -> SVG p i

-- | An SVG element that does not admit children.
type Leaf r p i
   = Array (IProp r i)
  -> SVG p i

type SVGAttributes = 
  GlobalAttributes
  ( DocumentEventAttributes
  ( version :: I
  , baseProfile :: I
  , x :: I
  , y :: I
  , width :: I
  , height :: I
  , preserveAspectRatio :: I
  , contentScriptType :: I
  , contentStyleType :: I
  , viewBox :: I ) )
svg :: forall p i. Node SVGAttributes p i
svg = refine E.svg

type GAttributes =
  GlobalAttributes ()
g :: forall p i. Node GAttributes p i
g = refine E.g

type PathAttributes =
  GlobalAttributes
  ( d :: I
  , pathLength :: I
  , transform :: I )
path :: forall p i. Node PathAttributes p i
path = refine E.path

type CircleAttributes =
  GlobalAttributes
  ( cx :: I
  , cy :: I
  , r :: I
  , transform :: I )
circle :: forall p i. Node CircleAttributes p i
circle = refine E.circle

type DefsAttributes =
  GlobalAttributes
  ( cx :: I
  , cy :: I
  , r :: I
  , transform :: I )
defs :: forall p i. Node DefsAttributes p i
defs = refine E.defs

type FilterAttributes =
  GlobalAttributes
  ( x :: I
  , y :: I
  , width :: I
  , height :: I
  , filterRes :: I
  , filterUnits :: I
  , primitiveUnits :: I
  , href :: I )
filter :: forall p i. Node FilterAttributes p i
filter = refine E.filter

type FeGaussianBlurAttributes =
  GlobalAttributes
  ( in :: I
  , stdDeviation :: I )
feGaussianBlur :: forall p i. Node FeGaussianBlurAttributes p i
feGaussianBlur = refine E.feGaussianBlur

type ClipPathAttributes =
  GlobalAttributes
  ( clipPathUnits :: I )
clipPath :: forall p i. Node ClipPathAttributes p i
clipPath = refine E.clipPath

type LinearGradientAttributes =
  GlobalAttributes
  ( gradientUnits :: I
  , gradientTransform :: I
  , x1 :: I
  , y1 :: I
  , x2 :: I
  , y2 :: I
  , spreadmethod :: I
  , href :: I )
linearGradient :: forall p i. Node LinearGradientAttributes p i
linearGradient = refine E.linearGradient

type StopAttributes =
  GlobalAttributes
  ( offset :: I )
stop :: forall p i. Node StopAttributes p i
stop = refine E.stop