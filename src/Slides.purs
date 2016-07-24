module Slides (slides) where

import Data.Semigroup ((<>))

import Slides.Title as Title
import Slides.BiasedGraph as BiasedGraph
import Pokeball as Pokeball
import ForceTest as ForceTest

import Slides.Util

slides :: forall b p i. Array (TTween b (HTML p i))
slides 
  = Title.slides
  <> BiasedGraph.slides
  <> [ForceTest.forceTest, Pokeball.pokeball]