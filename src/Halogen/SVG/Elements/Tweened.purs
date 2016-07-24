module Halogen.SVG.Elements.Tweened where

import Halogen.HTML.Properties.Indexed (IProp(), I())

import Halogen.SVG.Elements.Indexed as I

import Halogen.HTML.Elements.Tweened (Node, ttweenNode)

svg :: forall b p i. Node b I.SVGAttributes p i
svg = ttweenNode I.svg

g :: forall b p i. Node b I.GAttributes p i
g = ttweenNode I.g

path :: forall b p i. Node b I.PathAttributes p i
path = ttweenNode I.path

circle :: forall b p i. Node b I.CircleAttributes p i
circle = ttweenNode I.circle

rect :: forall b p i. Node b I.RectAttributes p i
rect = ttweenNode I.rect

defs :: forall b p i. Node b I.DefsAttributes p i
defs = ttweenNode I.defs

filter :: forall b p i. Node b I.FilterAttributes p i
filter = ttweenNode I.filter

feGaussianBlur :: forall b p i. Node b I.FeGaussianBlurAttributes p i
feGaussianBlur = ttweenNode I.feGaussianBlur

clipPath :: forall b p i. Node b I.ClipPathAttributes p i
clipPath = ttweenNode I.clipPath