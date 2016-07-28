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

ttweenLeaf :: forall b a c r. (Array a -> r) -> Array (TTween b a) ->TTween b r
ttweenLeaf f attrs = do
  attrs' <- sequence <$> sequence attrs
  pure $ f <$> attrs'

text :: forall b p i. String -> TTween b (HH.HTML p i)
text = HH.text >>> pure >>> pure

type DivProperties = 
  ( InteractiveEvents 
  ( GlobalProperties 
  ( onScroll :: I )))
div :: forall b p i. Node b DivProperties p i
div = ttweenNode HH.div

type SpanProperties = 
  ( InteractiveEvents 
  ( GlobalProperties ()))
span :: forall b p i. Node b SpanProperties p i
span = ttweenNode HH.span

type PProperties = 
  ( InteractiveEvents 
  ( GlobalProperties 
  ( onScroll :: I )))
p :: forall b p i. Node b PProperties p i
p = ttweenNode HH.p

type UlProperties = 
  ( InteractiveEvents 
  ( GlobalProperties 
  ( onScroll :: I )))
ul :: forall b p i. Node b UlProperties p i
ul = ttweenNode HH.ul

type LiProperties = 
  ( InteractiveEvents 
  ( GlobalProperties 
  ( value :: I, onScroll :: I )))
li :: forall b p i. Node b LiProperties p i
li = ttweenNode HH.li

type ImgProperties = 
  ( InteractiveEvents 
  ( GlobalProperties 
  ( alt :: I
  , crossorigin :: I
  , height :: I
  , ismap :: I
  , longdesc :: I
  , onAbort :: I
  , onError :: I
  , onLoad :: I
  , src :: I
  , usemap :: I
  , width :: I ) ) )
img :: forall b p i. Leaf b ImgProperties p i
img = ttweenLeaf HH.img
