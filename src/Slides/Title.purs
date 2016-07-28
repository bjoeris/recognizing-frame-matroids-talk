module Slides.Title (slides) where

import Slides.Util

slides :: forall b p i. Array (TTween b (HTML p i))
slides = 
  [ titleSlide 
    [ title "Recognizing Frame Matroids" 
    , authors
      [ text "R. Chen"
      , instituteRef "1"
      , text " J. Geelen"
      , instituteRef "2"
      , text " "
      , speaker "B. Joeris"
      , instituteRef "2"
      , text " P. Nelson"
      , instituteRef "2"
      ]
    , institutes
      [ instituteRef "1"
      , text "Fuzhou University"
      , text "  "
      , instituteRef "2"
      , text "University of Waterloo"
      ]
    , conference 
      [ div [] [ text "2016 International Workshop on Structure" ]
      , div [] [ text "in Graphs and Matroids" ]
      ]
    ]
  ]

titleSlide :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
titleSlide = div [class_ ("title-slide")]

title :: forall b p i. String -> TTween b (HTML p i)
title s = div [class_ ("talk-title")] [ text s ]

authors :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
authors = div [class_ ("author")]

instituteRef :: forall b p i. String -> TTween b (HTML p i)
instituteRef s = span [class_ ("institute-ref")] [ text s ]

speaker :: forall b p i. String -> TTween b (HTML p i)
speaker s = span [class_ ("speaker")] [ text s ]

institutes :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
institutes = div [class_ ("institute")]

institute :: forall b p i. String -> String -> TTween b (HTML p i)
institute ref inst = span [class_ ("institute")]
  [ instituteRef ref
  , text inst ]

conference :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
conference c = div [class_ ("conference")] c