module Slides (slides) where

import Data.Semigroup ((<>))

import Slides.Title as Title
import Slides.BiasedGraph as BiasedGraph
import Slides.Connectivity as Connectivity
import Slides.Thanks as Thanks
import Pokeball as Pokeball
import ForceTest as ForceTest

import Slides.Util

slides :: forall b p i. Array (TTween b (HTML p i))
slides 
  = Title.slides
  <> BiasedGraph.slides
  <> Connectivity.slides
  <> Thanks.slides