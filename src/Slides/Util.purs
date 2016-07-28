module Slides.Util 
  ( slide
  , p_
  , li_
  , emph
  , redText
  , blueText
  , unlines
  , svgOverlay
  , overlayParent
  , wideOverlayParent
  , overlay
  , slideTitle
  , slideTitle'
  , lemma
  , theorem
  , namedTheorem
  , module Halogen.HTML
  , module Halogen.HTML.Elements.Tweened
  , module Halogen.HTML.Styles.Tweened
  , module Halogen.HTML.Properties.Tweened
  , module Halogen.SVG.Elements
  , module Halogen.SVG.Elements.Tweened
  , module Halogen.SVG.Properties.Tweened
  , module Halogen.CSS.Colors
  , module Halogen.CSS.Units
  , module Timeline.Build
  , module Timeline.Tween
  , module Timeline.Slides
  , module Data.Maybe
  , module Data.Array
  , module Data.Monoid
  ) where

import Prelude hiding (id, bottom)

import Halogen.HTML(HTML(), ClassName(), className)
import Halogen.HTML.Elements.Tweened
  ( text
  , div
  , span
  , p
  , ul
  , li
  , img
  )
import Halogen.HTML.Properties.Tweened
  ( src
  )
import Halogen.HTML.Styles.Tweened
  ( opacity
  , backgroundColor
  , absolute
  , position
  , left
  , right
  , top
  , bottom
  , width
  , height
  , marginLeft
  , marginRight
  , marginBottom
  , marginTop
  )

import Halogen.SVG.Elements.Tweened
  ( svg
  , g
  , path
  , rect
  , circle
  , defs
  , filter
  , feGaussianBlur
  , clipPath
  )

import Halogen.SVG.Elements (SVG)
import Halogen.SVG.Properties.Tweened

import Halogen.CSS.Colors

import Halogen.CSS.Units

import Timeline.Slides (fadeIn, fadeIn', fadeOut, fadeOut', fadeInOut, fadeInOut')

import Timeline.Build (Timeline, newStep, newStep')
import Timeline.Tween (TTween(), ttween, ttween')

import Data.String as String

import Data.Maybe (Maybe(Nothing,Just),maybe,fromMaybe)
import Data.Array ((\\))
import Data.Monoid ((<>))

slide :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
slide = div []

p_ :: forall b p i. String -> TTween b (HTML p i)
p_ s = p [] [text s]

li_ :: forall b p i. String -> TTween b (HTML p i)
li_ s = li [] [text s]

emph :: forall b p i. String -> TTween b (HTML p i)
emph s = span [class_ "emph"] [text s]

unlines :: Array String -> String
unlines = String.joinWith "\n"

overlayParent :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
overlayParent = div [ class_ "overlay-parent" ]

wideOverlayParent :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
wideOverlayParent = div [ class_ "wide-overlay-parent" ]

overlay :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
overlay = div [ class_ "overlay-child" ]

svgOverlay :: forall b p i. Array (TTween b (SVG p i)) -> TTween b (HTML p i)
svgOverlay = svg 
  [ class_ "overlay-child"
  , viewBox 0.0 0.0 1.0 1.0
  , preserveAspectRatio {defer: false, align: Nothing, meetOrSlice: Nothing} 
  ]

slideTitle :: forall b p i. String -> TTween b (HTML p i)
slideTitle s = div [class_ "slide-title"] [text s]

slideTitle' :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
slideTitle' c = div [class_ "slide-title"] c

redText :: forall b p i. String -> TTween b (HTML p i)
redText s = span [class_ "highlight-red"] [text s]

blueText :: forall b p i. String -> TTween b (HTML p i)
blueText s = span [class_ "highlight-blue"] [text s]

lemma :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
lemma c = p [class_ "theorem"] $
  [ p [class_ "theorem-title"] [text "Lemma. "] ]
  <> c

theorem :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
theorem c = p [class_ "theorem"] $
  [ p [class_ "theorem-title"] [text "Theorem. "] ]
  <> c

namedTheorem :: forall b p i. String -> Array (TTween b (HTML p i)) -> TTween b (HTML p i)
namedTheorem name c = p [class_ "theorem"] $
  [ p [class_ "theorem-title"]
    [ text "Theorem "
    , span [class_ "theorem-name"] 
      [ text $ "(" <> name <> ")" ]
    , text ". "
    ]
  ] <> c