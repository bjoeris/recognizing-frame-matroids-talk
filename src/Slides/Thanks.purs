module Slides.Thanks (slides) where

import Slides.Util

slides :: forall b p i. Array (TTween b (HTML p i))
slides = 
  [ titleSlide 
    [ title "Thank You" ]
  ]

titleSlide :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
titleSlide = div [class_ ("title-slide")]

title :: forall b p i. String -> TTween b (HTML p i)
title s = div [class_ ("talk-title")] [ text s ]
