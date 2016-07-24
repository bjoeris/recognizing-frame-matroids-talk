module Halogen.HTML.Styles where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))

import Halogen.HTML.Core (Prop(Prop), prop, propName, propNames, attrName)

import Halogen.CSS.Colors (class IsColor, toColorString)
import Halogen.CSS.Units (class IsLength, toLengthString)

lengthStyle :: forall v i. IsLength v => String -> v -> Prop i
lengthStyle name =
  prop (propNames ["style", name]) Nothing <<< toLengthString

colorStyle :: forall v i. String -> IsColor v => v -> Prop i
colorStyle name = prop (propNames ["style", name]) Nothing <<< toColorString

style :: forall i. String -> Prop i
style = prop (propName "style") (Just $ attrName "style")

opacity :: forall i. Number -> Prop i
opacity = prop (propNames ["style", "opacity"]) Nothing

-- fill :: forall i. String -> Prop i
-- fill = prop (propNames ["style", "fill"]) (Just (attrName "fill"))

-- stroke :: forall i. String -> Prop i
-- stroke = prop (propNames ["style", "stroke"]) (Just (attrName "stroke"))

-- strokeWidth :: forall i. String -> Prop i
-- strokeWidth = prop (propNames ["style", "strokeWidth"]) (Just (attrName "stroke-width"))

backgroundColor :: forall v i. IsColor v => v -> Prop i
backgroundColor = colorStyle "background-color"

visibility :: forall i. String -> Prop i
visibility = prop (propNames ["style", "visibility"]) Nothing

overflow :: forall i. String -> Prop i
overflow = prop (propNames ["style", "overflow"]) Nothing

height :: forall v i. IsLength v => v -> Prop i
height = lengthStyle "height"

width :: forall v i. IsLength v => v -> Prop i
width = lengthStyle "width"

left :: forall v i. IsLength v => v -> Prop i
left = lengthStyle "left"

right :: forall v i. IsLength v => v -> Prop i
right = lengthStyle "right"

top :: forall v i. IsLength v => v -> Prop i
top = lengthStyle "top"

bottom :: forall v i. IsLength v => v -> Prop i
bottom = lengthStyle "bottom"

data Position
  = Static
  | Relative
  | Absolute
  | Fixed
  | Sticky

static :: Position
static = Static

relative :: Position
relative = Relative

absolute :: Position
absolute = Absolute

fixed :: Position
fixed = Fixed

sticky :: Position
sticky = Sticky

toPositionString :: Position -> String
toPositionString Static = "static"
toPositionString Relative = "relative"
toPositionString Absolute = "absolute"
toPositionString Fixed = "fixed"
toPositionString Sticky = "sticky"

position :: forall i. Position -> Prop i
position = prop (propNames ["style", "position"]) Nothing <<< toPositionString