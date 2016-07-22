module Halogen.HTML.Elements.Tweened where

import Prelude

import Data.Traversable (sequence)

import Halogen.HTML.Core (Prop(), prop, propName, propNames, attrName)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (IProp(), I(), InteractiveEvents, GlobalProperties)

import Timeline.Tween (TTween, Tween, tween)

type Node b r p i
   = Array (TTween b ((IProp r) i))
    -> Array (TTween b (HH.HTML p i))
    -> TTween b (HH.HTML p i)
    
type Leaf b r p i
   = Array (TTween b ((IProp r) i))
    -> TTween b (HH.HTML p i)

ttweenNode :: forall b a c r. (Array a -> Array c -> r) -> Array (TTween b a) -> Array (TTween b c) -> TTween b r
ttweenNode f attrs children = do
  attrs' <- sequence <$> sequence attrs
  children' <- sequence <$> sequence children
  pure $ f <$> attrs' <*> children'

text :: forall b p i. String -> TTween b (HH.HTML p i)
text = HH.text >>> pure >>> pure

type DivProperties = 
  ( InteractiveEvents 
  ( GlobalProperties 
  ( onScroll :: I )))
div :: forall b p i. Node b DivProperties p i
div = ttweenNode HH.div