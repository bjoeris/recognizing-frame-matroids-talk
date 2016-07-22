module Halogen.HTML.Styles where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))

import Halogen.HTML.Core (Prop(Prop), prop, propName, propNames, attrName)

import Halogen.CSS.Colors (class IsColor, toColorString)

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

colorStyle :: forall v i. String -> IsColor v => v -> Prop i
colorStyle name = prop (propNames ["style", name]) Nothing <<< toColorString

backgroundColor :: forall v i. IsColor v => v -> Prop i
backgroundColor = colorStyle "background-color"