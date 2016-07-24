module Halogen.SVG.Elements where

import Prelude ((<<<),($))

import Data.Maybe (Maybe(Just,Nothing))

import Halogen.HTML.Core (HTML(Element), Prop(), TagName(), Namespace(), tagName, namespace)

type SVG p i = HTML p i

svgNS :: Namespace
svgNS = namespace "http://www.w3.org/2000/svg"
--svgNS = namespace "svg"

element :: forall p i. String -> Array (Prop i) -> Array (SVG p i) -> SVG p i 
element = Element (Just svgNS) <<< tagName 

svg :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
svg = Element (Just svgNS) $ tagName "svg"

path :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
path = element "path"

g :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
g = element "g"

circle :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
circle = element "circle"

rect :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
rect = element "rect"

defs :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
defs = element "defs"

filter :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
filter = element "filter"

feGaussianBlur :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
feGaussianBlur = element "feGaussianBlur"

clipPath :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
clipPath = element "clipPath"

linearGradient :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
linearGradient = element "linearGradient"

stop :: forall p i. Array (Prop i) -> Array (SVG p i) -> SVG p i
stop = element "stop"